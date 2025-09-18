unit test_yookassa_webhook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, yookassa_webhook
  ;

type
  { TTestYookassaWebhook }

  TTestYookassaWebhook = class(TTestCase)
  private
    FHandler: TYookassaWebhookHandler;
    FLogMsg: string;
    FLogEventType: TEventType;
    FReceivedEvent: TYookassaWebhookData;
    procedure LogHandler(aEvent: TEventType; const aMsg: string);
    procedure OnPaymentSucceeded(const aEvent: TYookassaWebhookData);
    procedure OnPaymentWaitingForCapture(const aEvent: TYookassaWebhookData);
    procedure OnPaymentCanceled(const aEvent: TYookassaWebhookData);
    procedure OnRefundSucceeded(const aEvent: TYookassaWebhookData);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHandleWebhook_PaymentSucceeded;
    procedure TestHandleWebhook_PaymentWaitingForCapture;
    procedure TestHandleWebhook_PaymentCanceled;
    procedure TestHandleWebhook_RefundSucceeded;
    procedure TestHandleWebhook_InvalidJSON;
    procedure TestHandleWebhook_UnknownEvent;
    procedure TestHandleWebhook_Logging;
    // New tests for PaymentResponse property and object type detection
    procedure TestWebhookData_PaymentResponse_Properties;
    procedure TestWebhookData_ObjectType_Payment;
    procedure TestWebhookData_ObjectType_Refund;
    procedure TestWebhookData_ObjectType_Unknown;
    procedure TestWebhookData_Clone;
    procedure TestWebhookData_Event_Property;
    procedure TestWebhookData_PaymentResponse_Creation;
    procedure TestWebhookData_PaymentResponse_Null_When_No_Object;
    procedure TestWebhookException_Creation;
  end;

implementation

uses
  StrUtils, yookassa_responses, fpjson
  ;

{ TTestYookassaWebhook }

procedure TTestYookassaWebhook.SetUp;
begin
  inherited SetUp;
  FHandler := TYookassaWebhookHandler.Create;
  FHandler.OnLog := @LogHandler;
  FLogMsg := '';
  FLogEventType := etDebug;
  FReceivedEvent := nil;
end;

procedure TTestYookassaWebhook.TearDown;
begin
  FHandler.Free;
  FReceivedEvent.Free;
  inherited TearDown;
end;

procedure TTestYookassaWebhook.LogHandler(aEvent: TEventType; const aMsg: string);
begin
  FLogEventType := aEvent;
  FLogMsg := aMsg;
end;

procedure TTestYookassaWebhook.OnPaymentSucceeded(const aEvent: TYookassaWebhookData);
begin
  FReceivedEvent := aEvent.Clone; // Copy link for checking
end;

procedure TTestYookassaWebhook.OnPaymentWaitingForCapture(const aEvent: TYookassaWebhookData);
begin
  FReceivedEvent := aEvent.Clone;
end;

procedure TTestYookassaWebhook.OnPaymentCanceled(const aEvent: TYookassaWebhookData);
begin
  FReceivedEvent := aEvent.Clone;
end;

procedure TTestYookassaWebhook.OnRefundSucceeded(const aEvent: TYookassaWebhookData);
begin
  FReceivedEvent := aEvent.Clone;
end;

procedure TTestYookassaWebhook.TestHandleWebhook_PaymentSucceeded;
const
  RawBody = '{' +
    '"event":"payment.succeeded",' +
    '"object":{' +
      '"id":"pay_123abc",' +
      '"type":"payment",' +
      '"status":"succeeded",' +
      '"amount":{"value":"100.00","currency":"RUB"},' +
      '"confirmation":{"confirmation_url":"https://yookassa.ru/checkout/pay/test"}' +
    '}' +
  '}';
var
  aResult: string;
begin
  FHandler.OnPaymentSucceeded := @OnPaymentSucceeded;
  aResult := FHandler.HandleWebhook(RawBody);

  AssertEquals('{"status": "ok"}', aResult);
  AssertNotNull('Event should be received', FReceivedEvent);
  AssertEquals('payment.succeeded', FReceivedEvent.Event);
  AssertEquals(Ord(wotPayment), Ord(FReceivedEvent.ObjectType));
  AssertEquals('pay_123abc', FReceivedEvent.PaymentResponse.ID);
  AssertEquals(Ord(psSucceeded), Ord(FReceivedEvent.PaymentResponse.Status));
end;

procedure TTestYookassaWebhook.TestHandleWebhook_PaymentWaitingForCapture;
const
  RawBody = '{' +
    '"event":"payment.waiting_for_capture",' +
    '"object":{' +
      '"id":"pay_456def",' +
      '"type":"payment",' +
      '"status":"waiting_for_capture",' +
      '"amount":{"value":"200.00","currency":"RUB"}' +
    '}' +
  '}';
var
  aResult: string;
begin
  FHandler.OnPaymentWaitingForCapture := @OnPaymentWaitingForCapture;
  aResult := FHandler.HandleWebhook(RawBody);

  AssertEquals('{"status": "ok"}', aResult);
  AssertNotNull('Event should be received', FReceivedEvent);
  AssertEquals('payment.waiting_for_capture', FReceivedEvent.Event);
  AssertEquals('pay_456def', FReceivedEvent.PaymentResponse.ID);
  AssertEquals(Ord(psWaitingForCapture), Ord(FReceivedEvent.PaymentResponse.Status));
end;

procedure TTestYookassaWebhook.TestHandleWebhook_PaymentCanceled;
const
  RawBody = '{' +
    '"event":"payment.canceled",' +
    '"object":{' +
      '"id":"pay_789ghi",' +
      '"type":"payment",' +
      '"status":"canceled"' +
    '}' +
  '}';
var
  aResult: string;
begin
  FHandler.OnPaymentCanceled := @OnPaymentCanceled;
  aResult := FHandler.HandleWebhook(RawBody);

  AssertEquals('{"status": "ok"}', aResult);
  AssertNotNull('Event should be received', FReceivedEvent);
  AssertEquals('payment.canceled', FReceivedEvent.Event);
  AssertEquals(Ord(psCanceled), Ord(FReceivedEvent.PaymentResponse.Status));
end;

procedure TTestYookassaWebhook.TestHandleWebhook_RefundSucceeded;
const
  RawBody = '{' +
    '"event":"refund.succeeded",' +
    '"object":{' +
      '"id":"rfnd_999",' +
      '"type":"refund",' +
      '"status":"succeeded"' +
    '}' +
  '}';
var
  aResult: string;
begin
  FHandler.OnRefundSucceeded := @OnRefundSucceeded;
  aResult := FHandler.HandleWebhook(RawBody);

  AssertEquals('{"status": "ok"}', aResult);
  AssertNotNull('Event should be received', FReceivedEvent);
  AssertEquals('refund.succeeded', FReceivedEvent.Event);
  AssertEquals(Ord(wotRefund), Ord(FReceivedEvent.ObjectType));
end;

procedure TTestYookassaWebhook.TestHandleWebhook_InvalidJSON;
var
  aResult: string;
begin
  aResult := FHandler.HandleWebhook('{"invalid": json}');
  AssertTrue('Response should indicate error', Pos('error', aResult) > 0);
  AssertTrue('Should log invalid JSON error', StartsStr('Webhook: Invalid JSON - ', FLogMsg));
end;

procedure TTestYookassaWebhook.TestHandleWebhook_UnknownEvent;
const
  RawBody = '{' +
    '"event":"unknown.event",' +
    '"object":{' +
      '"id":"obj_123",' +
      '"type":"unknown",' +
      '"status":"unknown"' +
    '}' +
  '}';
var
  aResult: string;
begin
  aResult := FHandler.HandleWebhook(RawBody);
  AssertEquals('{"status": "ok"}', aResult);
  AssertNull('No event handler should be called', FReceivedEvent);
end;

procedure TTestYookassaWebhook.TestHandleWebhook_Logging;
const
  RawBody = '{' +
    '"event":"payment.succeeded",' +
    '"object":{' +
      '"id":"pay_log_test",' +
      '"type":"payment",' +
      '"status":"succeeded"' +
    '}' +
  '}';
begin
  FHandler.OnPaymentSucceeded := @OnPaymentSucceeded;
  FHandler.HandleWebhook(RawBody);

  AssertTrue('Should log debug for received body', Pos('Webhook: Triggering handler for', FLogMsg) > 0);
  AssertEquals(Ord(etInfo), Ord(FLogEventType));
end;

procedure TTestYookassaWebhook.TestWebhookData_PaymentResponse_Properties;
const
  RawBody = '{' +
    '"event":"payment.succeeded",' +
    '"object":{' +
      '"id":"pay_test_props",' +
      '"type":"payment",' +
      '"status":"succeeded",' +
      '"amount":{"value":"150.75","currency":"RUB"},' +
      '"confirmation":{"confirmation_url":"https://yookassa.ru/checkout/pay/props"}' +
    '}' +
  '}';
var
  aJSON: TJSONObject;
  aWebhookData: TYookassaWebhookData;
  aPaymentResp: TYookassaPaymentResponse;
begin
  aJSON := TJSONObject(GetJSON(RawBody));
  aWebhookData := TYookassaWebhookData.Create(aJSON, False);
  try
    aPaymentResp := aWebhookData.PaymentResponse;
    AssertNotNull('PaymentResponse should be created', aPaymentResp);
    AssertEquals('Payment ID should match', 'pay_test_props', aPaymentResp.ID);
    AssertEquals('Payment status should be succeeded', Ord(psSucceeded), Ord(aPaymentResp.Status));
    AssertEquals('Payment amount should match', 150.75, aPaymentResp.Amount);
    AssertEquals('Payment currency should match', 'RUB', aPaymentResp.CurrencyCode);
    AssertEquals('Confirmation URL should match', 'https://yookassa.ru/checkout/pay/props', aPaymentResp.ConfirmationURL);
  finally
    aWebhookData.Free;
    aJSON.Free;
  end;
end;

procedure TTestYookassaWebhook.TestWebhookData_ObjectType_Payment;
const
  RawBody = '{' +
    '"event":"payment.succeeded",' +
    '"object":{"id":"pay_123","type":"payment"}' +
  '}';
var
  aJSON: TJSONObject;
  aWebhookData: TYookassaWebhookData;
begin
  aJSON := TJSONObject(GetJSON(RawBody));
  aWebhookData := TYookassaWebhookData.Create(aJSON, True);
  try
    AssertEquals('Object type should be payment', Ord(wotPayment), Ord(aWebhookData.ObjectType));
    AssertEquals('Event should match', 'payment.succeeded', aWebhookData.Event);
  finally
    aWebhookData.Free;
  end;
end;

procedure TTestYookassaWebhook.TestWebhookData_ObjectType_Refund;
const
  RawBody = '{' +
    '"event":"refund.succeeded",' +
    '"object":{"id":"rfnd_123","type":"refund"}' +
  '}';
var
  aWebhookData: TYookassaWebhookData;
begin
  aWebhookData := TYookassaWebhookData.Create(TJSONObject(GetJSON(RawBody)), True);
  try
    AssertEquals('Object type should be refund', Ord(wotRefund), Ord(aWebhookData.ObjectType));
    AssertEquals('Event should match', 'refund.succeeded', aWebhookData.Event);
  finally
    aWebhookData.Free;
  end;
end;

procedure TTestYookassaWebhook.TestWebhookData_ObjectType_Unknown;
const
  RawBody = '{' +
    '"event":"custom.event",' +
    '"object":{"id":"obj_123","type":"custom"}' +
  '}';
var
  aWebhookData: TYookassaWebhookData;
begin
  aWebhookData := TYookassaWebhookData.Create(TJSONObject(GetJSON(RawBody)), True);
  try
    AssertEquals('Object type should be unknown', Ord(wotUnknown), Ord(aWebhookData.ObjectType));
    AssertEquals('Event should match', 'custom.event', aWebhookData.Event);
  finally
    aWebhookData.Free;
  end;
end;

procedure TTestYookassaWebhook.TestWebhookData_Clone;
const
  RawBody = '{' +
    '"event":"payment.succeeded",' +
    '"object":{' +
      '"id":"pay_clone_test",' +
      '"type":"payment",' +
      '"status":"succeeded"' +
    '}' +
  '}';
var
  aOriginal, aClone: TYookassaWebhookData;
begin
  aOriginal := TYookassaWebhookData.Create(TJSONObject(GetJSON(RawBody)), True);
  try
    aClone := aOriginal.Clone;
    try
      AssertNotNull('Clone should not be nil', aClone);
      AssertTrue('Clone should be different object', aOriginal <> aClone);
      AssertEquals('Clone event should match original', aOriginal.Event, aClone.Event);
      AssertEquals('Clone object type should match original', Ord(aOriginal.ObjectType), Ord(aClone.ObjectType));

      // Test that both can access PaymentResponse independently
      AssertNotNull('Original PaymentResponse should work', aOriginal.PaymentResponse);
      AssertNotNull('Clone PaymentResponse should work', aClone.PaymentResponse);
      AssertEquals('Both should have same payment ID',
        aOriginal.PaymentResponse.ID, aClone.PaymentResponse.ID);
    finally
      aClone.Free;
    end;
  finally
    aOriginal.Free;
  end;
end;

procedure TTestYookassaWebhook.TestWebhookData_Event_Property;
const
  RawBody = '{' +
    '"event":"payment.waiting_for_capture",' +
    '"object":{"id":"pay_123","type":"payment"}' +
  '}';
var
  aWebhookData: TYookassaWebhookData;
begin
  aWebhookData := TYookassaWebhookData.Create(TJSONObject(GetJSON(RawBody)), True);
  try
    AssertEquals('Event property should return correct value',
      'payment.waiting_for_capture', aWebhookData.Event);
  finally
    aWebhookData.Free;
  end;
end;

procedure TTestYookassaWebhook.TestWebhookData_PaymentResponse_Creation;
const
  RawBody = '{' +
    '"event":"payment.succeeded",' +
    '"object":{' +
      '"id":"pay_creation_test",' +
      '"type":"payment",' +
      '"status":"succeeded",' +
      '"amount":{"value":"99.99","currency":"USD"}' +
    '}' +
  '}';
var
  aWebhookData: TYookassaWebhookData;
  aPaymentResp1, aPaymentResp2: TYookassaPaymentResponse;
begin
  aWebhookData := TYookassaWebhookData.Create(TJSONObject(GetJSON(RawBody)), True);
  try
    // Test lazy creation and caching
    aPaymentResp1 := aWebhookData.PaymentResponse;
    aPaymentResp2 := aWebhookData.PaymentResponse;

    AssertNotNull('First call should create PaymentResponse', aPaymentResp1);
    AssertNotNull('Second call should return cached PaymentResponse', aPaymentResp2);
    AssertTrue('Both calls should return same instance', aPaymentResp1 = aPaymentResp2);

    AssertEquals('Payment ID should be correct', 'pay_creation_test', aPaymentResp1.ID);
    AssertEquals('Payment amount should be correct', 99.99, aPaymentResp1.Amount);
    AssertEquals('Payment currency should be correct', 'USD', aPaymentResp1.CurrencyCode);
  finally
    aWebhookData.Free;
  end;
end;

procedure TTestYookassaWebhook.TestWebhookData_PaymentResponse_Null_When_No_Object;
const
  RawBody = '{"event":"payment.succeeded"}';
var
  aWebhookData: TYookassaWebhookData;
begin
  aWebhookData := TYookassaWebhookData.Create(TJSONObject(GetJSON(RawBody)), True);
  try
    AssertNull('PaymentResponse should be nil when no object in webhook',
      aWebhookData.PaymentResponse);
  finally
    aWebhookData.Free;
  end;
end;

procedure TTestYookassaWebhook.TestWebhookException_Creation;
var
  aException: EYooKassaWebhookError;
begin
  aException := EYooKassaWebhookError.Create('Test webhook error', 'WEBHOOK_ERROR');
  try
    AssertEquals('Message should match', 'Test webhook error', aException.Message);
    AssertEquals('Error code should match', 'WEBHOOK_ERROR', aException.ErrorCode);
  finally
    aException.Free;
  end;
end;

initialization
  RegisterTest(TTestYookassaWebhook);

end.
