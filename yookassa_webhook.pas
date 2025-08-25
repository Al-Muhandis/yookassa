unit yookassa_webhook;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, yookassa_responses
  ;

type
  { Event handler for logging }
  TYookassaLogEvent = procedure(aEvent: TEventType; const aMsg: string) of object;

  { Exception for YooKassa webhook errors }
  EYooKassaWebhookError = class(Exception)
  private
    FErrorCode: string;
  public
    constructor Create(const aMsg, aErrorCode: string);
    property ErrorCode: string read FErrorCode;
  end;

  { Webhook object types based on event analysis }
  TYookassaWebhookObjectType = (
    wotNone,
    wotUnknown,
    wotPayment,
    wotRefund,
    wotPayout,
    wotDeal,
    wotPaymentMethod
  );

  { TYookassaWebhookData - наследуется от TYookassaResponse для единообразия архитектуры }
  TYookassaWebhookData = class(TYookassaObject)
  private
    FObjectType: TYookassaWebhookObjectType;
    FPaymentResponse: TYookassaPaymentResponse;
    function GetEvent: string;
    function GetObjectType: TYookassaWebhookObjectType;
    function GetPaymentResponse: TYookassaPaymentResponse;
    function DetermineObjectType(const aEvent: string): TYookassaWebhookObjectType;
    class function CreateResponseFromObject(const aObjectJSON: TJSONObject;
      aObjectType: TYookassaWebhookObjectType): TYookassaResponse;
  public
    destructor Destroy; override;
    function Clone: TYookassaWebhookData;

    { Webhook-specific properties }
    property Event: string read GetEvent;
    property ObjectType: TYookassaWebhookObjectType read GetObjectType;

    { Typed access to webhook objects }
    property PaymentResponse: TYookassaPaymentResponse read GetPaymentResponse;

    { TODO: Add support for other webhook object types
    property RefundResponse: TYookassaRefundResponse read GetRefundResponse;
    property PayoutResponse: TYookassaPayoutResponse read GetPayoutResponse;
    property DealResponse: TYookassaDealResponse read GetDealResponse;
    property PaymentMethodResponse: TYookassaPaymentMethodResponse read GetPaymentMethodResponse;
    }
  end;

  { Event handler type for webhook events }
  TYookassaWebhookEvent = procedure(const aEvent: TYookassaWebhookData) of object;

  { TYookassaWebhookHandler }
  TYookassaWebhookHandler = class
  private
    FEventHandlers: array[TYookassaWebhookObjectType] of record
      OnSucceeded: TYookassaWebhookEvent;
      OnWaitingForCapture: TYookassaWebhookEvent;
      OnCanceled: TYookassaWebhookEvent;
      OnActive: TYookassaWebhookEvent;
      OnClosed: TYookassaWebhookEvent;
    end;
    FOnLog: TYookassaLogEvent;
    procedure Log(aEventType: TEventType; const aMsg: string);
    function ProcessWebhookEvent(const aEvent: TYookassaWebhookData): Boolean;
  public
    function HandleWebhook(const aRawBody: string): string;

    { Payment events }
    property OnPaymentSucceeded: TYookassaWebhookEvent
      read FEventHandlers[wotPayment].OnSucceeded
      write FEventHandlers[wotPayment].OnSucceeded;
    property OnPaymentWaitingForCapture: TYookassaWebhookEvent
      read FEventHandlers[wotPayment].OnWaitingForCapture
      write FEventHandlers[wotPayment].OnWaitingForCapture;
    property OnPaymentCanceled: TYookassaWebhookEvent
      read FEventHandlers[wotPayment].OnCanceled
      write FEventHandlers[wotPayment].OnCanceled;

    { Refund events }
    property OnRefundSucceeded: TYookassaWebhookEvent
      read FEventHandlers[wotRefund].OnSucceeded
      write FEventHandlers[wotRefund].OnSucceeded;

    { TODO: Add other event properties
    property OnPayoutSucceeded: TYookassaWebhookEvent
      read FEventHandlers[wotPayout].OnSucceeded
      write FEventHandlers[wotPayout].OnSucceeded;
    property OnPayoutCanceled: TYookassaWebhookEvent
      read FEventHandlers[wotPayout].OnCanceled
      write FEventHandlers[wotPayout].OnCanceled;
    property OnDealClosed: TYookassaWebhookEvent
      read FEventHandlers[wotDeal].OnClosed
      write FEventHandlers[wotDeal].OnClosed;
    property OnPaymentMethodActive: TYookassaWebhookEvent
      read FEventHandlers[wotPaymentMethod].OnActive
      write FEventHandlers[wotPaymentMethod].OnActive;
    }

    property OnLog: TYookassaLogEvent read FOnLog write FOnLog;
  end;

implementation

uses
  StrUtils;

{ EYooKassaWebhookError }

constructor EYooKassaWebhookError.Create(const aMsg, aErrorCode: string);
begin
  inherited Create(aMsg);
  FErrorCode := aErrorCode;
end;

{ TYookassaWebhookData }

function TYookassaWebhookData.DetermineObjectType(const aEvent: string): TYookassaWebhookObjectType;
begin
  if StartsStr('payment.', aEvent) then
    Result := wotPayment
  else if StartsStr('refund.', aEvent) then
    Result := wotRefund
  else if StartsStr('payout.', aEvent) then
    Result := wotPayout
  else if StartsStr('deal.', aEvent) then
    Result := wotDeal
  else if StartsStr('payment_method.', aEvent) then
    Result := wotPaymentMethod
  else
    Result := wotUnknown;
end;

class function TYookassaWebhookData.CreateResponseFromObject(const aObjectJSON: TJSONObject;
  aObjectType: TYookassaWebhookObjectType): TYookassaResponse;
begin
  case aObjectType of
    { aOwnsRaw = False, так как Raw владеет экземпляр TYookassaWebhookData }
    wotPayment: Result := TYookassaPaymentResponse.Create(aObjectJSON, False);
    { TODO: Add other response types
    wotRefund: Result := TYookassaRefundResponse.Create(aObjectJSON.Clone as TJSONObject);
    wotPayout: Result := TYookassaPayoutResponse.Create(aObjectJSON.Clone as TJSONObject);
    wotDeal: Result := TYookassaDealResponse.Create(aObjectJSON.Clone as TJSONObject);
    wotPaymentMethod: Result := TYookassaPaymentMethodResponse.Create(aObjectJSON.Clone as TJSONObject);
    }
    else
      Result := nil;
  end;
end;

function TYookassaWebhookData.GetPaymentResponse: TYookassaPaymentResponse;
var
  aObjectJSON: TJSONObject;
begin
  if not Assigned(FPaymentResponse) and (ObjectType = wotPayment) then
  begin
    aObjectJSON := Raw.Find('object') as TJSONObject;
    if Assigned(aObjectJSON) then
      FPaymentResponse := CreateResponseFromObject(aObjectJSON, wotPayment) as TYookassaPaymentResponse;
  end;
  Result := FPaymentResponse;
end;

function TYookassaWebhookData.GetEvent: string;
begin
  Result:=Raw.Get('event', '')
end;

function TYookassaWebhookData.GetObjectType: TYookassaWebhookObjectType;
begin
  if FObjectType=wotNone then
    FObjectType := DetermineObjectType(Event);
  Result:=FObjectType;
end;

destructor TYookassaWebhookData.Destroy;
begin
  FPaymentResponse.Free;
  inherited Destroy;
end;

function TYookassaWebhookData.Clone: TYookassaWebhookData;
begin
{ Раз клонируем весь экземпляр, то и Raw клонируем и владеть им будет клонированный экземпляр }
  Result := TYookassaWebhookData.Create(Raw.Clone as TJSONObject, True);
end;

{ TYookassaWebhookHandler }

procedure TYookassaWebhookHandler.Log(aEventType: TEventType; const aMsg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(aEventType, aMsg);
end;

function TYookassaWebhookHandler.ProcessWebhookEvent(const aEvent: TYookassaWebhookData): Boolean;
var
  aEventName: string;
  aHandler: TYookassaWebhookEvent;
begin
  Result := False;
  aEventName := ExtractDelimited(2, aEvent.Event, ['.']);

  case aEvent.ObjectType of
    wotPayment:
      begin
        case aEventName of
          'succeeded': aHandler := FEventHandlers[wotPayment].OnSucceeded;
          'waiting_for_capture': aHandler := FEventHandlers[wotPayment].OnWaitingForCapture;
          'canceled': aHandler := FEventHandlers[wotPayment].OnCanceled;
          else aHandler := nil;
        end;
      end;
    wotRefund:
      begin
        case aEventName of
          'succeeded': aHandler := FEventHandlers[wotRefund].OnSucceeded;
          else aHandler := nil;
        end;
      end;
    { TODO: Add other object types
    wotPayout: ...,
    wotDeal: ...,
    wotPaymentMethod: ...
    }
    else
      aHandler := nil;
  end;

  if Assigned(aHandler) then
  begin
    Log(etInfo, Format('Webhook: Triggering handler for %s', [aEvent.Event]));
    aHandler(aEvent);
    Result := True;
  end;
end;

function TYookassaWebhookHandler.HandleWebhook(const aRawBody: string): string;
var
  aJSON: TJSONObject;
  aData: TYookassaWebhookData;
begin
  Result := '{"error": "Bad Request"}';

  Log(etDebug, 'Webhook: Received raw body: ' + aRawBody);

  // 1. Parse JSON
  try
    aJSON := TJSONObject(GetJSON(aRawBody));
    Log(etDebug, 'Webhook: JSON parsed successfully');
  except
    on E: Exception do
    begin
      Log(etError, 'Webhook: Invalid JSON - ' + E.Message);
      Result := '{"error": "Invalid JSON"}';
      Exit;
    end;
  end;

  // Передаем aJSON объект на владение новому экземпляру TYookassaWebhookData
  aData := TYookassaWebhookData.Create(aJSON, True);
  try
    Log(etInfo, Format('Webhook: Processing event "%s"', [aData.Event]));

    // 3. Process event using unified handler system
    if not ProcessWebhookEvent(aData) then
      Log(etWarning, Format('Webhook: No handler found for event: %s', [aData.Event]));

    // 4. Return success response
    Result := '{"status": "ok"}';

  finally
    aData.Free;
  end;
end;

end.
