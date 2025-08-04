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
  end;

implementation

uses
  StrUtils
  ;

{ TTestYookassaWebhook }

procedure TTestYookassaWebhook.SetUp;
begin
  inherited SetUp;
  FHandler := TYookassaWebhookHandler.Create('test_secret');
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
      '"status":"succeeded"' +
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
  AssertEquals('payment', FReceivedEvent.ObjectType);
  AssertEquals('pay_123abc', FReceivedEvent.ObjectId);
  AssertEquals('succeeded', FReceivedEvent.Status);
end;

procedure TTestYookassaWebhook.TestHandleWebhook_PaymentWaitingForCapture;
const
  RawBody = '{' +
    '"event":"payment.waiting_for_capture",' +
    '"object":{' +
      '"id":"pay_456def",' +
      '"type":"payment",' +
      '"status":"waiting_for_capture"' +
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
  AssertEquals('pay_456def', FReceivedEvent.ObjectId);
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
  AssertEquals('canceled', FReceivedEvent.Status);
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
  AssertEquals('rfnd_999', FReceivedEvent.ObjectId);
end;

procedure TTestYookassaWebhook.TestHandleWebhook_InvalidJSON;
var
  aResult: string;
begin
  aResult := FHandler.HandleWebhook('{"invalid": json}');
  AssertTrue('Response should indicate error', Pos('error', aResult) > 0);
  AssertTrue(StartsStr('Webhook: Invalid JSON - ', FLogMsg)); // Log not called due to early exit
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
  AssertTrue('Should log warning for unknown event', Pos('Unknown or unhandled event', FLogMsg) > 0);
  AssertEquals(Ord(etWarning), Ord(FLogEventType));
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

  AssertTrue('Should log debug for received body', Pos('Triggering OnPaymentSucceeded', FLogMsg) > 0);
  AssertEquals(Ord(etInfo), Ord(FLogEventType));
end;

initialization
  RegisterTest(TTestYookassaWebhook);

end.

