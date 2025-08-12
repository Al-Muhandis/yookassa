unit yookassa_webhook;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

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

  { TYookassaWebhookData }
  TYookassaWebhookData = class(TPersistent)
  public
    Event: string;          // payment.succeeded, payment.waiting_for_capture, etc.
    ObjectType: string;     // payment, refund, payout, etc.
    ObjectId: string;       // Object ID (e.g. pay_123)
    Status: string;         // Object status
    Raw: TJSONObject;       // Full JSON for direct access
    function Clone: TYookassaWebhookData;
    constructor Create(aRaw: TJSONObject);
    destructor Destroy; override;
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

{ EYooKassaWebhookError }

constructor EYooKassaWebhookError.Create(const aMsg, aErrorCode: string);
begin
  inherited Create(aMsg);
  FErrorCode := aErrorCode;
end;

{ TYookassaWebhookData }

function TYookassaWebhookData.Clone: TYookassaWebhookData;
begin
  Result:=TYookassaWebhookData.Create(Raw.Clone as TJSONObject);
end;

constructor TYookassaWebhookData.Create(aRaw: TJSONObject);
var
  aObj: TJSONObject;
begin
  Raw := aRaw;
  aObj := aRaw.Find('object') as TJSONObject;
  Event := aRaw.Get('event', '');
  ObjectType := aObj.Get('type', '');
  ObjectId := aObj.Get('id', '');
  Status := aObj.Get('status', '');
end;

destructor TYookassaWebhookData.Destroy;
begin
  Raw.Free;
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
    Log(etInfo, Format('Webhook: Processing event "%s" for object "%s" (status: %s)',
      [aEventType, aData.ObjectId, aData.Status]));

    // 3. Call the appropriate event handler
    if Assigned(FOnPaymentSucceeded) and (aEventType = 'payment.succeeded') then
    begin
      Log(etInfo, 'Webhook: Triggering OnPaymentSucceeded');
      FOnPaymentSucceeded(aData);
    end
    else if Assigned(FOnPaymentWaitingForCapture) and (aEventType = 'payment.waiting_for_capture') then
    begin
      Log(etInfo, 'Webhook: Triggering OnPaymentWaitingForCapture');
      FOnPaymentWaitingForCapture(aData);
    end
    else if Assigned(FOnPaymentCanceled) and (aEventType = 'payment.canceled') then
    begin
      Log(etInfo, 'Webhook: Triggering OnPaymentCanceled');
      FOnPaymentCanceled(aData);
    end
    else if Assigned(FOnRefundSucceeded) and (aEventType = 'refund.succeeded') then
    begin
      Log(etInfo, 'Webhook: Triggering OnRefundSucceeded');
      FOnRefundSucceeded(aData);
    end
    else
    begin
      Log(etWarning, Format('Webhook: Unknown or unhandled event: %s', [aEventType]));
    end;

    // 4. Return success response
    Result := '{"status": "ok"}';

  finally
    aData.Free;
  end;
end;

end.
