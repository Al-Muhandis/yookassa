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

function WebhookObjectTypeToString(aObjectType: TYookassaWebhookObjectType): String;

implementation

uses
  StrUtils, yookassa_constants
  ;

const
  _WebhookObjectTypes: array[TYookassaWebhookObjectType] of String =
    (
      '',
      '',
      _WEBHOOK_OBJECT_TYPE_PAYMENT,
      _WEBHOOK_OBJECT_TYPE_REFUND,
      _WEBHOOK_OBJECT_TYPE_PAYOUT,
      _WEBHOOK_OBJECT_TYPE_DEAL,
      _WEBHOOK_OBJECT_TYPE_PAYMENT_METHOD
    );

function WebhookObjectTypeToString(aObjectType: TYookassaWebhookObjectType): String;
begin
  Result:=_WebhookObjectTypes[aObjectType];
end;

{ EYooKassaWebhookError }

constructor EYooKassaWebhookError.Create(const aMsg, aErrorCode: string);
begin
  inherited Create(aMsg);
  FErrorCode := aErrorCode;
end;

{ TYookassaWebhookData }

function TYookassaWebhookData.DetermineObjectType(const aEvent: string): TYookassaWebhookObjectType;
begin
  if StartsStr(_WEBHOOK_OBJECT_TYPE_PAYMENT+'.', aEvent) then
    Result := wotPayment
  else if StartsStr(_WEBHOOK_OBJECT_TYPE_REFUND+'.', aEvent) then
    Result := wotRefund
  else if StartsStr(_WEBHOOK_OBJECT_TYPE_PAYOUT+'.', aEvent) then
    Result := wotPayout
  else if StartsStr(_WEBHOOK_OBJECT_TYPE_DEAL+'.', aEvent) then
    Result := wotDeal
  else if StartsStr(_WEBHOOK_OBJECT_TYPE_PAYMENT_METHOD+'.', aEvent) then
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
    aObjectJSON := Raw.Find(_JSON_FIELD_OBJECT) as TJSONObject;
    if Assigned(aObjectJSON) then
      FPaymentResponse := CreateResponseFromObject(aObjectJSON, wotPayment) as TYookassaPaymentResponse;
  end;
  Result := FPaymentResponse;
end;

function TYookassaWebhookData.GetEvent: string;
begin
  Result:=Raw.Get(_JSON_FIELD_EVENT, EmptyStr)
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
          _PAYMENT_STATUS_SUCCEEDED:           aHandler := FEventHandlers[wotPayment].OnSucceeded;
          _PAYMENT_STATUS_WAITING_FOR_CAPTURE: aHandler := FEventHandlers[wotPayment].OnWaitingForCapture;
          _PAYMENT_STATUS_CANCELED:            aHandler := FEventHandlers[wotPayment].OnCanceled;
        else
          aHandler := nil;
        end;
      end;
    wotRefund:
      begin
        case aEventName of
          _REFUND_STATUS_SUCCEEDED: aHandler := FEventHandlers[wotRefund].OnSucceeded;
        else
          aHandler := nil;
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
    Log(etInfo, Format(_LOG_WEBHOOK_TRIGGERING, [aEvent.Event]));
    aHandler(aEvent);
    Result := True;
  end;
end;

function TYookassaWebhookHandler.HandleWebhook(const aRawBody: string): string;
var
  aJSON: TJSONObject;
  aData: TYookassaWebhookData;
  aJSONData: TJSONData;
begin
  Result := _RESPONSE_ERROR_BAD_REQUEST;
  aJSON := nil;
  aData := nil;

  Log(etDebug, Format(_LOG_WEBHOOK_RECEIVED, [aRawBody]));

  // 1. Parse JSON
  try
    aJSONData := GetJSON(aRawBody);
    if not (aJSONData is TJSONObject) then
    begin
      aJSONData.Free;
      Log(etError, _LOG_WEBHOOK_ROOT_NOT_OBJECT);
      Result := _RESPONSE_ERROR_INVALID_JSON_STRUCTURE;
      Exit;
    end;
    aJSON := aJSONData as TJSONObject;
    Log(etDebug, _LOG_WEBHOOK_JSON_PARSED);
  except
    on E: Exception do
    begin
      Log(etError, Format(_LOG_WEBHOOK_INVALID_JSON, [E.Message]));
      Result := _RESPONSE_ERROR_INVALID_JSON;
      Exit;
    end;
  end;

  // 2. Create WebhookData with exception safety
  try
    aData := TYookassaWebhookData.Create(aJSON, True);
    aJSON := nil; // Transfer ownership successfully
  except
    on E: Exception do
    begin
      aJSON.Free; // Cleanup JSON if WebhookData creation failed
      Log(etError, Format(_LOG_WEBHOOK_DATA_FAILED, [E.Message]));
      Result := _RESPONSE_ERROR_DATA_PROCESSING;
      Exit;
    end;
  end;
  try
    Log(etInfo, Format(_LOG_WEBHOOK_PROCESSING, [aData.Event]));

    // 3. Process event using unified handler system
    if not ProcessWebhookEvent(aData) then
      Log(etWarning, Format(_LOG_WEBHOOK_NO_HANDLER, [aData.Event]));

    // 4. Return success response
    Result := _RESPONSE_STATUS_OK;

  finally
    aData.Free;
  end;
end;

end.
