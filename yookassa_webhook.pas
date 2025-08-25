unit yookassa_webhook;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, yookassa_api
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
    wotUnknown,
    wotPayment,
    wotRefund,
    wotPayout,
    wotDeal,
    wotPaymentMethod
  );

  { TYookassaWebhookData }
  TYookassaWebhookData = class(TPersistent)
  private
    FPaymentResponse: TYookassaPaymentResponse;
    FObjectType: TYookassaWebhookObjectType;
    FRaw: TJSONObject;
    function GetPaymentResponse: TYookassaPaymentResponse;
    function DetermineObjectType(const aEvent: string): TYookassaWebhookObjectType;
  public
    Event: string;          // payment.succeeded, payment.waiting_for_capture, etc.

    function Clone: TYookassaWebhookData;
    constructor Create(aRaw: TJSONObject);
    destructor Destroy; override;

    property ObjectType: TYookassaWebhookObjectType read FObjectType; // Determined from event
    { Typed access to webhook objects }
    property PaymentResponse: TYookassaPaymentResponse read GetPaymentResponse;
    property Raw: TJSONObject read FRaw;       // Full JSON for direct access

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
    FOnPaymentSucceeded: TYookassaWebhookEvent;
    FOnPaymentWaitingForCapture: TYookassaWebhookEvent;
    FOnPaymentCanceled: TYookassaWebhookEvent;
    FOnRefundSucceeded: TYookassaWebhookEvent;
    { TODO: Add handlers for other events
    FOnPayoutSucceeded: TYookassaWebhookEvent;
    FOnPayoutCanceled: TYookassaWebhookEvent;
    FOnDealClosed: TYookassaWebhookEvent;
    FOnPaymentMethodActive: TYookassaWebhookEvent;
    }
    FOnLog: TYookassaLogEvent;
    procedure Log(aEventType: TEventType; const aMsg: string);
  public
    // Main method. Call this from any web server implementation.
    // Takes the raw request body and returns a JSON response string.
    function HandleWebhook(const aRawBody: string): string;
    property OnPaymentSucceeded: TYookassaWebhookEvent read FOnPaymentSucceeded write FOnPaymentSucceeded;
    property OnPaymentWaitingForCapture: TYookassaWebhookEvent read FOnPaymentWaitingForCapture write FOnPaymentWaitingForCapture;
    property OnPaymentCanceled: TYookassaWebhookEvent read FOnPaymentCanceled write FOnPaymentCanceled;
    property OnRefundSucceeded: TYookassaWebhookEvent read FOnRefundSucceeded write FOnRefundSucceeded;
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

function TYookassaWebhookData.GetPaymentResponse: TYookassaPaymentResponse;
var
  aObjectJSON: TJSONObject;
begin
  if not Assigned(FPaymentResponse) and (ObjectType = wotPayment) then
  begin
    aObjectJSON := Raw.Find('object') as TJSONObject;
    if Assigned(aObjectJSON) then
      FPaymentResponse := TYookassaPaymentResponse.Create(aObjectJSON.Clone as TJSONObject);
  end;
  Result := FPaymentResponse;
end;

function TYookassaWebhookData.Clone: TYookassaWebhookData;
begin
  Result := TYookassaWebhookData.Create(Raw.Clone as TJSONObject);
end;

constructor TYookassaWebhookData.Create(aRaw: TJSONObject);
var
  aObj: TJSONObject;
begin
  FRaw := aRaw;
  aObj := aRaw.Find('object') as TJSONObject;

  Event := aRaw.Get('event', EmptyStr);
  FObjectType := DetermineObjectType(Event);
end;

destructor TYookassaWebhookData.Destroy;
begin
  FPaymentResponse.Free;
  FRaw.Free;
  inherited Destroy;
end;

{ TYookassaWebhookHandler }

procedure TYookassaWebhookHandler.Log(aEventType: TEventType; const aMsg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(aEventType, aMsg);
end;

function TYookassaWebhookHandler.HandleWebhook(const aRawBody: string): string;
var
  aJSON: TJSONObject;
  aData: TYookassaWebhookData;
  aEventType: string;
begin
  Result := '{"error": "Bad Request"}'; // Default response

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

  // 2. Create data object
  aData := TYookassaWebhookData.Create(aJSON);
  try
    aEventType := aData.Event;
    Log(etInfo, Format('Webhook: Processing event "%s"', [aEventType]));

    // 3. Call the appropriate event handler
    case aEventType of
      'payment.succeeded':
        if Assigned(FOnPaymentSucceeded) then
        begin
          Log(etInfo, 'Webhook: Triggering OnPaymentSucceeded');
          FOnPaymentSucceeded(aData);
        end;
      'payment.waiting_for_capture':
        if Assigned(FOnPaymentWaitingForCapture) then
        begin
          Log(etInfo, 'Webhook: Triggering OnPaymentWaitingForCapture');
          FOnPaymentWaitingForCapture(aData);
        end;
      'payment.canceled':
        if Assigned(FOnPaymentCanceled) then
        begin
          Log(etInfo, 'Webhook: Triggering OnPaymentCanceled');
          FOnPaymentCanceled(aData);
        end;
      'refund.succeeded':
        if Assigned(FOnRefundSucceeded) then
        begin
          Log(etInfo, 'Webhook: Triggering OnRefundSucceeded');
          FOnRefundSucceeded(aData);
        end;
      { TODO: Add other event handlers
      'payout.succeeded': ...,
      'payout.canceled': ...,
      'deal.closed': ...,
      'payment_method.active': ...
      }
      else
        Log(etWarning, Format('Webhook: Unknown or unhandled event: %s', [aEventType]));
    end;

    // 4. Return success response
    Result := '{"status": "ok"}';

  finally
    aData.Free;
  end;
end;

end.
