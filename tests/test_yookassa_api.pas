unit test_yookassa_api;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, yookassa_responses, yookassa_models, yookassa_requests
  ;

type

  { TTestableCreatePayment }

  TTestableCreatePayment = class(TYookassaCreatePaymentRequest)
  public
    procedure TestToJSON;
  end;

  { TTestableCreateReceiptRequest }

  TTestableCreateReceiptRequest = class(TYookassaCreateReceiptRequest);

  { TTestableGetPaymentRequest }

  TTestableGetPaymentRequest = class(TYookassaGetPaymentRequest)
  private
    class var FTestRaw: TJSONObject;
  protected
    function DoExecute: String; override;
    function CreateResponse(aRaw: TJSONObject): TYookassaResponse; override;
    class property TestRaw: TJSONObject read FTestRaw write FTestRaw;
  end;

  { TTestYooKassa }

  TTestYooKassa = class(TTestCase)
  private
    FPaymentRequest: TTestableCreatePayment;
    FReceiptRequest: TTestableCreateReceiptRequest;
    FTestItem: TYookassaReceiptItem;
    procedure CallToJSON;
    procedure FillPaymentData(aPayment: TYookassaCreatePaymentRequest);
    procedure LogHandler(aEvent: TEventType; const aMsg: string);
    procedure CreatePaymentStaticHandler;
    class procedure AddProductToReceipt(aReceipt: TYookassaReceipt; const aProductDescription: String; aVatCode: Integer=1;
      aTaxSystemCode: Integer=-1);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBuildRequestData;
    procedure TestCreatePaymentStatic;
    procedure TestGetPaymentRequest_BuildRequestJSON;
    procedure TestGetPaymentRequest_CreateResponse;
    procedure TestGetPayment_StaticMethod;
    procedure TestPaymentResponseParsing;
    procedure TestReceiptEmptyCustomer;
    procedure TestReceiptItemsAutoFree;
    procedure TestReceiptItemToJSON;
    procedure TestReceiptToJSON;
    procedure TestReceiptWithPhoneToJSON;
    procedure TestReceiptWithTaxSystemCodeToJSON;
    procedure TestReceiptRequestBuildJSON;
    procedure TestReceiptRequestBuildRefundJSON;
    procedure TestReceiptRequestCreate;
    procedure TestReceiptResponseParsing;
    procedure TestReceiver_BankAccount_ToJSON;
    procedure TestReceiver_MobileBalance_ToJSON;
    procedure TestReceiver_DigitalWallet_ToJSON;
    procedure TestReceiver_Validation_MissingFields;
    procedure TestSettlements_ToJSON;
    procedure TestSupplier_InReceiptItemToJSON;
    procedure TestLogging;
    procedure TestMarkCodeInfoValidation;
    procedure TestPhoneValidation;
    procedure TestEmailValidation;
    procedure TestEmailAndPhoneValidationTogether;
  end;

implementation

uses
  yookassa_exceptions
  ;

{ TTestableCreatePayment }

procedure TTestableCreatePayment.TestToJSON;
begin
  ToJSON.Free;
end;

{ TTestableGetPaymentRequest }

function TTestableGetPaymentRequest.DoExecute: String;
begin
  if not Assigned(FTestRaw) then
    raise Exception.Create(Format('Set up json RAW for %s', [Self.ClassName]));
  Result:=FTestRaw.AsJSON;
end;

function TTestableGetPaymentRequest.CreateResponse(aRaw: TJSONObject): TYookassaResponse;
begin
  Result:=inherited CreateResponse(aRaw);
end;

{ TTestYooKassa }

procedure TTestYooKassa.CallToJSON;
var
  aJSON: TJSONObject;
begin
  aJSON:=FTestItem.ToJSON;
  aJSON.Free;
end;

procedure TTestYooKassa.FillPaymentData(aPayment: TYookassaCreatePaymentRequest);
begin
  aPayment.Amount := 12300.55;
  aPayment.Currency := 'RUB';
  aPayment.Description := 'Test payment';
  aPayment.ReturnUrl:='https://sample.com/';
end;

procedure TTestYooKassa.LogHandler(aEvent: TEventType; const aMsg: string);
begin
  AssertEquals('Test message', aMsg);
  AssertEquals(Ord(etDebug), Ord(aEvent));
end;

procedure TTestYooKassa.CreatePaymentStaticHandler;
begin
  TYookassaCreatePaymentRequest.CreatePayment('', '', 100000, 'RUB', 'Test', 'https://return');
end;

class procedure TTestYooKassa.AddProductToReceipt(aReceipt: TYookassaReceipt; const aProductDescription: String;
  aVatCode: Integer; aTaxSystemCode: Integer);
var
  aItem: TYookassaReceiptItem;
begin
  aReceipt.Customer.Email := 'test@example.com';
  aReceipt.TaxSystemCode:=aTaxSystemCode;

  aItem := TYookassaReceiptItem.Create;
  aItem.Description := aProductDescription;
  aItem.Quantity := 1;
  aItem.AmountValue := 1000000.00;
  aItem.AmountCurrency := 'RUB';
  aItem.VatCode := aVatCode;
  aReceipt.AddItem(aItem);
end;

procedure TTestYooKassa.SetUp;
begin
  FPaymentRequest := TTestableCreatePayment.Create;
  FReceiptRequest := TTestableCreateReceiptRequest.Create;
  inherited SetUp;
end;

procedure TTestYooKassa.TearDown;
begin
  FPaymentRequest.Free;
  FReceiptRequest.Free;
  inherited TearDown;
end;

procedure TTestYooKassa.TestBuildRequestData;
var
  o: TJSONObject;
  aJSON: TJSONStringType;
begin
  FillPaymentData(FPaymentRequest);
  o := FPaymentRequest.ToJSON;
  try
    aJSON:=o.AsJSON;
  finally
    o.Free;
  end;
  AssertTrue(Pos('"amount"', aJSON) > 0);
  AssertTrue(Pos('"value" : "12300.55"', aJSON) > 0);
end;

procedure TTestYooKassa.TestCreatePaymentStatic;
begin
  AssertException(EYooKassaValidationError, @CreatePaymentStaticHandler);
end;

procedure TTestYooKassa.TestGetPaymentRequest_BuildRequestJSON;
var
  aReq: TTestableGetPaymentRequest;
  o: TJSONObject;
begin
  aReq := TTestableGetPaymentRequest.Create;
  try
    o:=aReq.ToJSON;
    try
      AssertNull('GET request should have no body', o);
    finally
      o.Free;
    end;
  finally
    aReq.Free;
  end;
end;

procedure TTestYooKassa.TestGetPaymentRequest_CreateResponse;
var
  aReq: TTestableGetPaymentRequest;
  aRaw: TJSONObject;
  aResp: TYookassaPaymentResponse;
begin
  aRaw := TJSONObject.Create;
  try
    aRaw.Add('id', 'pay_123');
    aRaw.Add('status', 'succeeded');
    aRaw.Add('amount', TJSONObject.Create(['value', '100.00', 'currency', 'RUB']));

    aReq := TTestableGetPaymentRequest.Create;
    try
      aResp := aReq.CreateResponse(aRaw) as TYookassaPaymentResponse;
      try
        AssertEquals('pay_123', aResp.ID);
        AssertEquals(Ord(psSucceeded), Ord(aResp.Status));
        AssertEquals(100.00, (aResp).Amount);
      finally
        aResp.Free;
      end;
    finally
      aReq.Free;
    end;
  finally
    // aRaw destroying in Destroy aResp
  end;
end;

procedure TTestYooKassa.TestGetPayment_StaticMethod;
var
  aResp: TYookassaPaymentResponse;
  aRaw, aConfirmation: TJSONObject;
begin
  // Replacing Execute (in reality, you need to get mock, but a stub will do for the unit test)
  // We can't call the real API, so we'll check that if Execute returns JSON, everything will be parsed.

  // Creating a fake response
  aRaw := TJSONObject.Create;
  TTestableGetPaymentRequest.FTestRaw := aRaw;
  try
    aRaw.Add('id', 'pay_test_999');
    aRaw.Add('status', 'waiting_for_capture');
    aRaw.Add('amount', TJSONObject.Create(['value', '500.00', 'currency', 'RUB']));
    aConfirmation := TJSONObject.Create;
    aConfirmation.Add('confirmation_url', 'https://yookassa.ru/checkout/pay/test');
    aRaw.Add('confirmation', aConfirmation);

    // Call static method
    aResp := TTestableGetPaymentRequest.GetPayment('test_shop', 'test_key', 'pay_test_999');
    try
      AssertEquals('pay_test_999', aResp.ID);
      AssertEquals(Ord(psWaitingForCapture), Ord(aResp.Status));
      AssertEquals(500.00, aResp.Amount);
      AssertEquals('https://yookassa.ru/checkout/pay/test', aResp.ConfirmationURL);
    finally
      aResp.Free;
    end;
  finally
    FreeAndNil(TTestableGetPaymentRequest.FTestRaw);
  end;
end;

procedure TTestYooKassa.TestLogging;
begin
  FPaymentRequest.OnLog := @LogHandler;
  FPaymentRequest.Log(etDebug, 'Test message');
end;

procedure TTestYooKassa.TestPaymentResponseParsing;
var
  aRaw, aConfirmation, aAmount: TJSONObject;
  aResp: TYookassaPaymentResponse;
begin
  aRaw := TJSONObject.Create;
  try
    aRaw.Add('id', 'pay_123');
    aRaw.Add('status', 'pending');
    aConfirmation := TJSONObject.Create;
    aConfirmation.Add('confirmation_url', 'https://yookassa.ru/checkout/pay/abc');
    aRaw.Add('confirmation', aConfirmation);
    aAmount := TJSONObject.Create;
    aAmount.Add('value', '100.00');
    aAmount.Add('currency', 'RUB');
    aRaw.Add('amount', aAmount);

{ OwnsRaw = False, потому что объект aRaw освобождаем самостоятельно}
    aResp := TYookassaPaymentResponse.Create(aRaw, False);
    try
      AssertEquals('pay_123', aResp.ID);
      AssertEquals(Ord(psPending), Ord(aResp.Status));
      AssertEquals('https://yookassa.ru/checkout/pay/abc', aResp.ConfirmationURL);
      AssertEquals(100.00, aResp.Amount);
    finally
      aResp.Free;
    end;
  finally
    aRaw.Free;
  end;
end;

procedure TTestYooKassa.TestReceiptItemToJSON;
var
  aItem: TYookassaReceiptItem;
  aJSON: TJSONObject;
begin
  aItem := TYookassaReceiptItem.Create;
  try
    aItem.Description := 'Товар';
    aItem.Quantity := 2.0;
    aItem.AmountValue := 150.00;
    aItem.AmountCurrency := 'RUB';
    aItem.VatCode := 1;
    aItem.PaymentMode := pmFullPrepayment;
    aItem.PaymentSubject := 'commodity';
    aItem.Measure := 'piece';

    aJSON := aItem.ToJSON;
    try
      AssertEquals('Товар', aJSON.Strings['description']);
      AssertEquals(2.0, aJSON.Floats['quantity']);
      AssertTrue(aJSON.Find('amount') <> nil);
      AssertEquals('RUB', TJSONObject(aJSON.Objects['amount']).Strings['currency']);
      AssertEquals('full_prepayment', aJSON.Strings['payment_mode']);
      AssertEquals('commodity', aJSON.Strings['payment_subject']);
      AssertEquals('piece', aJSON.Strings['measure']);
    finally
      aJSON.Free;
    end;
  finally
    aItem.Free;
  end;
end;

procedure TTestYooKassa.TestReceiptToJSON;
var
  aReceipt: TYookassaReceipt;
  aItem: TYookassaReceiptItem;
  aJSON: TJSONObject;
  aItems: TJSONArray;
begin
  aReceipt := TYookassaReceipt.Create;
  try
    aReceipt.Customer.Email := 'user@example.com';
    aItem := TYookassaReceiptItem.Create;
    aItem.Description := 'Позиция 1';
    aItem.Quantity := 1;
    aItem.AmountValue := 100.00;
    aItem.AmountCurrency := 'RUB';
    aItem.VatCode := 2;
    aReceipt.AddItem(aItem);
    aJSON := aReceipt.ToJSON;
    try
      AssertTrue(aJSON.Find('customer') <> nil);
      AssertEquals('user@example.com', TJSONObject(aJSON.Objects['customer']).Strings['email']);
      AssertTrue(aJSON.Find('items') <> nil);
      aItems := TJSONArray(aJSON.Arrays['items']);
      AssertEquals(1, aItems.Count);
      AssertEquals('Позиция 1', TJSONObject(aItems.Objects[0]).Strings['description']);
      AssertEquals(100.00, TJSONObject(TJSONObject(aItems.Objects[0]).Objects['amount']).Floats['value']);
    finally
      aJSON.Free;
    end;
  finally
    aReceipt.Free;
  end;
end;

procedure TTestYooKassa.TestReceiptWithPhoneToJSON;
var
  aReceipt: TYookassaReceipt;
  aItem: TYookassaReceiptItem;
  aJSON: TJSONObject;
  aCustomer: TJSONObject;
begin
  aReceipt := TYookassaReceipt.Create;
  try
    aReceipt.Customer.Email := 'user@example.com';
    aReceipt.Customer.Phone := '+79001234567';

    aItem := TYookassaReceiptItem.Create;
    aItem.Description := 'Товар с телефоном';
    aItem.Quantity := 1;
    aItem.AmountValue := 50.00;
    aItem.AmountCurrency := 'RUB';
    aItem.VatCode := 1;
    aReceipt.AddItem(aItem);

    aJSON := aReceipt.ToJSON;
    try
      AssertTrue(aJSON.Find('customer') <> nil);
      aCustomer := TJSONObject(aJSON.Objects['customer']);
      AssertEquals('user@example.com', aCustomer.Strings['email']);
      AssertEquals('79001234567', aCustomer.Strings['phone']);
    finally
      aJSON.Free;
    end;
  finally
    aReceipt.Free;
  end;
end;

procedure TTestYooKassa.TestReceiptWithTaxSystemCodeToJSON;
var
  aReceipt: TYookassaReceipt;
  aJSON: TJSONObject;
begin
  aReceipt := TYookassaReceipt.Create;
  try
    AddProductToReceipt(aReceipt, 'Товар с налогом', 2, 1);

    aJSON := aReceipt.ToJSON;
    try
      AssertEquals(1, aJSON.Integers['tax_system_code']);
    finally
      aJSON.Free;
    end;
  finally
    aReceipt.Free;
  end;
end;

procedure TTestYooKassa.TestReceiptRequestBuildJSON;
var
  aJSON: TJSONObject;
begin
  AddProductToReceipt(FReceiptRequest.Receipt, 'Тестовый товар', 1);

  FReceiptRequest.ReceiptType := rtPayment;
  FReceiptRequest.Send := True;

  aJSON := FReceiptRequest.ToJSON;
  try
    AssertEquals('payment', aJSON.Strings['type']);
    AssertEquals(True, aJSON.Booleans['send']);
  finally
    aJSON.Free;
  end;
end;

procedure TTestYooKassa.TestReceiptRequestBuildRefundJSON;
var
  aJSON: TJSONObject;
begin
  AddProductToReceipt(FReceiptRequest.Receipt, 'Возврат товара', 2);

  FReceiptRequest.ReceiptType := rtRefund;
  FReceiptRequest.PaymentId := 'payment_123456';
  FReceiptRequest.Send := False;

  aJSON := FReceiptRequest.ToJSON;
  try
    AssertEquals('refund', aJSON.Strings['type']);
    AssertEquals(False, aJSON.Booleans['send']);
    AssertEquals('payment_123456', aJSON.Strings['payment_id']);
  finally
    aJSON.Free;
  end;
end;

procedure TTestYooKassa.TestReceiptRequestCreate;
begin
  AddProductToReceipt(FReceiptRequest.Receipt, 'Создание чека', 1, 2);

  FReceiptRequest.ShopId := 'test_shop_id';
  FReceiptRequest.SecretKey := 'test_secret_key';
  FReceiptRequest.ReceiptType := rtPayment;
  FReceiptRequest.Send := True;

  AssertEquals('test_shop_id', FReceiptRequest.ShopId);
  AssertEquals('test_secret_key', FReceiptRequest.SecretKey);
  AssertEquals(Ord(rtPayment), Ord(FReceiptRequest.ReceiptType));
  AssertEquals(True, FReceiptRequest.Send);
  AssertTrue(Assigned(FReceiptRequest.Receipt));
end;

procedure TTestYooKassa.TestReceiptResponseParsing;
var
  aRaw: TJSONObject;
  aResp: TYookassaReceiptResponse;
begin
  aRaw := TJSONObject.Create;
  try
    aRaw.Add('id', 'rcpt_456');
    aRaw.Add('status', 'succeeded');
    aRaw.Add('payment_id', 'pay_789');

    aResp := TYookassaReceiptResponse.Create(aRaw, False);
    try
      AssertEquals('rcpt_456', aResp.ID);
      AssertEquals('succeeded', aResp.Status);
      AssertEquals('pay_789', aResp.PaymentId);
    finally
      aResp.Free;
    end;
  finally
    aRaw.Free;
  end;
end;

procedure TTestYooKassa.TestReceiver_BankAccount_ToJSON;
var
  aPayment: TTestableCreatePayment;
  aJSON, aReceiver: TJSONObject;
begin
  aPayment := TTestableCreatePayment.Create;
  try
    FillPaymentData(aPayment);

    aPayment.Receiver.ReceiverType := rtBankAccount;
    aPayment.Receiver.AccountNumber := '12345678901234567890';
    aPayment.Receiver.Bic := '044525225';

    aJSON := aPayment.ToJSON;
    try
      AssertTrue('receiver must be in JSON', Assigned(aJSON.Find('receiver')));
      aReceiver := aJSON.Objects['receiver'];
      AssertEquals('bank_account', aReceiver.Get('type', ''));
      AssertEquals('12345678901234567890', aReceiver.Get('account_number', ''));
      AssertEquals('044525225', aReceiver.Get('bic', ''));
    finally
      aJSON.Free;
    end;
  finally
    aPayment.Free;
  end;
end;

procedure TTestYooKassa.TestReceiver_MobileBalance_ToJSON;
var
  aPayment: TTestableCreatePayment;
  aJSON, aReceiver: TJSONObject;
begin
  aPayment := TTestableCreatePayment.Create;
  try
    FillPaymentData(aPayment);
    aPayment.Receiver.ReceiverType := rtMobileBalance;
    aPayment.Receiver.Phone := '79001234567';

    aJSON := aPayment.ToJSON;
    try
      AssertTrue('receiver must be in JSON', Assigned(aJSON.Find('receiver')));
      aReceiver := aJSON.Objects['receiver'];
      AssertEquals('mobile_balance', aReceiver.Get('type', ''));
      AssertEquals('79001234567', aReceiver.Get('phone', ''));
      AssertFalse('account_number must not be present', Assigned(aReceiver.Find('account_number')));
      AssertFalse('bic must not be present', Assigned(aReceiver.Find('bic')));
    finally
      aJSON.Free;
    end;
  finally
    aPayment.Free;
  end;
end;

procedure TTestYooKassa.TestReceiver_DigitalWallet_ToJSON;
var
  aPayment: TTestableCreatePayment;
  aJSON, aReceiver: TJSONObject;
begin
  aPayment := TTestableCreatePayment.Create;
  try
    FillPaymentData(aPayment);
    aPayment.Receiver.ReceiverType := rtDigitalWallet;
    aPayment.Receiver.AccountNumber := 'W1234567890';

    aJSON := aPayment.ToJSON;
    try
      AssertTrue('receiver must be in JSON', Assigned(aJSON.Find('receiver')));
      aReceiver := aJSON.Objects['receiver'];
      AssertEquals('digital_wallet', aReceiver.Get('type', ''));
      AssertEquals('W1234567890', aReceiver.Get('account_number', ''));
      AssertFalse('bic must not be present', Assigned(aReceiver.Find('bic')));
      AssertFalse('phone must not be present', Assigned(aReceiver.Find('phone')));
    finally
      aJSON.Free;
    end;
  finally
    aPayment.Free;
  end;
end;

procedure TTestYooKassa.TestReceiver_Validation_MissingFields;
var
  aPayment: TTestableCreatePayment;
begin
  aPayment := TTestableCreatePayment.Create;
  try
    FillPaymentData(aPayment);
    // An attempt to serialize without required fields
    aPayment.Receiver.ReceiverType := rtBankAccount;
    aPayment.Receiver.AccountNumber := '';
    aPayment.Receiver.Bic := '044525225';
    AssertException(EYooKassaValidationError, @aPayment.TestToJSON);

    aPayment.Receiver.AccountNumber := '12345678901234567890';
    aPayment.Receiver.Bic := '';
    AssertException(EYooKassaValidationError, @aPayment.TestToJSON);

    aPayment.Receiver.ReceiverType := rtMobileBalance;
    aPayment.Receiver.Phone := '';
    AssertException(EYooKassaValidationError, @aPayment.TestToJSON);

    aPayment.Receiver.ReceiverType := rtDigitalWallet;
    aPayment.Receiver.AccountNumber := '';
    AssertException(EYooKassaValidationError, @aPayment.TestToJSON);
  finally
    aPayment.Free;
  end;
end;

procedure TTestYooKassa.TestSettlements_ToJSON;
var
  aReceiptReq: TTestableCreateReceiptRequest;
  aSettlement: TYookassaSettlement;
  aJSON: TJSONObject;
  aSettlementsArray: TJSONArray;
  aSettlementJSON: TJSONObject;
begin
  aReceiptReq := TTestableCreateReceiptRequest.Create;
  try
    // add two calculations
    aSettlement := TYookassaSettlement.Create(stCashless, 500.00, 'RUB');
    aReceiptReq.Settlements.Add(aSettlement);

    aSettlement := TYookassaSettlement.Create(stPrepayment, 1500.00, 'RUB');
    aReceiptReq.Settlements.Add(aSettlement);

    AddProductToReceipt(aReceiptReq.Receipt, 'Некий продукт');

    aJSON := aReceiptReq.ToJSON;
    try
      AssertTrue('Settlements array must be in JSON', Assigned(aJSON.Find('settlements')));
      aSettlementsArray := aJSON.Arrays['settlements'];
      AssertEquals(2, aSettlementsArray.Count);

      aSettlementJSON := aSettlementsArray.Objects[0];
      AssertEquals('cashless', aSettlementJSON.Get('type', ''));
      AssertEquals('500.00', aSettlementJSON.Objects['amount'].Get('value', ''));

      aSettlementJSON := aSettlementsArray.Objects[1];
      AssertEquals('prepayment', aSettlementJSON.Get('type', ''));
      AssertEquals('1500.00', aSettlementJSON.Objects['amount'].Get('value', ''));
      AssertEquals('RUB', aSettlementJSON.Objects['amount'].Get('currency', ''));
    finally
      aJSON.Free;
    end;
  finally
    aReceiptReq.Free;
  end;
end;

procedure TTestYooKassa.TestSupplier_InReceiptItemToJSON;
var
  aItem: TYookassaReceiptItem;
  aJSON, aSupplierJSON: TJSONObject;
begin
  aItem := TYookassaReceiptItem.Create;
  try
    // Fill product data
    aItem.Description := 'Товар с поставщиком';
    aItem.Quantity := 1.0;
    aItem.AmountValue := 999.99;
    aItem.AmountCurrency := 'RUB';
    aItem.VatCode := 1;

    // Fill Supplier (must be created automatically)
    aItem.Supplier.Name := 'ООО "Ромашка"';
    aItem.Supplier.Phone := '74951234567'; // Указывается в формате ITU-T E.164, например 79000000000
    aItem.Supplier.Inn := '7701234567';

    // Stream to JSON
    aJSON := aItem.ToJSON;
    try
      // Check existance of a supplier in JSON
      AssertTrue('Item must contain "supplier"', Assigned(aJSON.Find('supplier')));

      aSupplierJSON := aJSON.Objects['supplier'];
      AssertEquals('Supplier name must match', 'ООО "Ромашка"', aSupplierJSON.Get('name', ''));
      AssertEquals('Supplier phone must match', '74951234567', aSupplierJSON.Get('phone', ''));
      AssertEquals('Supplier INN must match', '7701234567', aSupplierJSON.Get('inn', ''));
    finally
      aJSON.Free;
    end;
  finally
    aItem.Free;
  end;
end;

procedure TTestYooKassa.TestReceiptEmptyCustomer;
var
  aReceipt: TYookassaReceipt;
  aItem: TYookassaReceiptItem;
  aJSON: TJSONObject;
begin
  aReceipt := TYookassaReceipt.Create;
  try
    aItem := TYookassaReceiptItem.Create;
    aItem.Description := 'Товар без клиента';
    aItem.Quantity := 1;
    aItem.AmountValue := 75.00;
    aItem.AmountCurrency := 'RUB';
    aItem.VatCode := 1;
    aReceipt.AddItem(aItem);

    aJSON := aReceipt.ToJSON;
    try
      AssertTrue(aJSON.Find('customer') = nil);
      AssertTrue(aJSON.Find('items') <> nil);
    finally
      aJSON.Free;
    end;
  finally
    aReceipt.Free;
  end;
end;

procedure TTestYooKassa.TestReceiptItemsAutoFree;
var
  aReceipt: TYookassaReceipt;
  aItem: TYookassaReceiptItem;
  ItemCount: Integer;
begin
  ItemCount := 0;
  aItem := TYookassaReceiptItem.Create;
  aItem.Free; // check that works
  Inc(ItemCount);

  aReceipt := TYookassaReceipt.Create;
  try
    aItem := TYookassaReceiptItem.Create;
    aItem.Description := 'Автосвобождение';
    aReceipt.AddItem(aItem);
    // aItem now belongs to the list
  finally
    aReceipt.Free; // Must be call Free of all items
  end;
end;

procedure TTestYooKassa.TestMarkCodeInfoValidation;
var
  aItem: TYookassaReceiptItem;
  aJSON: TJSONObject;
begin
  aItem := TYookassaReceiptItem.Create;
  try
    aItem.Description := 'Товар с маркировкой';
    aItem.Quantity := 1;
    aItem.AmountValue := 100.00;
    aItem.AmountCurrency := 'RUB';
    aItem.VatCode := 1;
    aItem.MarkMode := 2; // Requires MarkCodeInfo

    FTestItem := aItem;
    // Empty MarkCodeInfo → error
    aItem.MarkCodeInfo := EmptyStr;
    AssertException(EYooKassaValidationError, @CallToJSON);

    // Invalid base64
    aItem.MarkCodeInfo := 'invalid_base64_%%%';
    AssertException(EYooKassaValidationError, @CallToJSON);

    // too short after decoding
    aItem.MarkCodeInfo := 'AA=='; // 2 bytes
    AssertException(EYooKassaValidationError, @CallToJSON);

    // valid base64 (length > 30)
    aItem.MarkCodeInfo := 'AQIDBAUGBwgJCgsMDQ4PEBESExQVFhcYGRobHB0eHyAhIiMkJSYnKCkqKywtLi8wMTIzNDU2Nzg5Ojs8PT4/P0BBQUJD';
    aJSON:=aItem.ToJSON;
    try
      AssertNotNull(aJSON); // must be passed
    finally
      aJSON.Free;
    end;
  finally
    aItem.Free;
  end;
end;

procedure TTestYooKassa.TestPhoneValidation;
var
  User: TYookassaUser;
begin
  User := TYookassaUser.Create;
  try
    // Валидные форматы
    User.Phone := '+7 (900) 123-45-67';
    AssertEquals('79001234567', User.Phone);

    User.Phone := '8 900 123 45 67';
    AssertEquals('79001234567', User.Phone);

    User.Phone := '+79001234567';
    AssertEquals('79001234567', User.Phone);

    User.Phone := '79001234567';
    AssertEquals('79001234567', User.Phone);

    // Пустой номер должен работать
    User.Phone := '';
    AssertEquals('', User.Phone);

    // Невалидный номер должен вызывать исключение
    try
      User.Phone := '123'; // Слишком короткий
      Fail('Should raise validation error');
    except
      on EYooKassaValidationError do
        ; // Expected
    end;

  finally
    User.Free;
  end;
end;

procedure TTestYooKassa.TestEmailValidation;
var
  User: TYookassaUser;
begin
  User := TYookassaUser.Create;
  try
    // Валидные форматы email с нормализацией
    User.Email := 'User@Example.Com';
    AssertEquals('user@example.com', User.Email);

    User.Email := 'user_name@sub.domain.org';
    AssertEquals('user_name@sub.domain.org', User.Email);

    User.Email := 'test-email@domain-name.com';
    AssertEquals('test-email@domain-name.com', User.Email);

    // Пустой email должен работать
    User.Email := '';
    AssertEquals('', User.Email);

    // Невалидные email должны вызывать исключение
    try
      User.Email := 'invalid-email'; // Нет @
      Fail('Should raise validation error for missing @');
    except
      on EYooKassaValidationError do
        ; // Expected
    end;

    try
      User.Email := 'user@@domain.com'; // Двойной @
      Fail('Should raise validation error for double @');
    except
      on EYooKassaValidationError do
        ; // Expected
    end;

    try
      User.Email := '@domain.com'; // Нет локальной части
      Fail('Should raise validation error for missing local part');
    except
      on EYooKassaValidationError do
        ; // Expected
    end;

    try
      User.Email := 'user@'; // Нет доменной части
      Fail('Should raise validation error for missing domain');
    except
      on EYooKassaValidationError do
        ; // Expected
    end;

    try
      User.Email := 'user@domain'; // Нет TLD
      Fail('Should raise validation error for missing TLD');
    except
      on EYooKassaValidationError do
        ; // Expected
    end;

    try
      User.Email := '.user@domain.com'; // Локальная часть начинается с точки
      Fail('Should raise validation error for local part starting with dot');
    except
      on EYooKassaValidationError do
        ; // Expected
    end;

    try
      User.Email := 'user.@domain.com'; // Локальная часть заканчивается точкой
      Fail('Should raise validation error for local part ending with dot');
    except
      on EYooKassaValidationError do
        ; // Expected
    end;

    try
      User.Email := 'user@.domain.com'; // Домен начинается с точки
      Fail('Should raise validation error for domain starting with dot');
    except
      on EYooKassaValidationError do
        ; // Expected
    end;

    try
      User.Email := 'user@domain.com.'; // Домен заканчивается точкой
      Fail('Should raise validation error for domain ending with dot');
    except
      on EYooKassaValidationError do
        ; // Expected
    end;

    try
      User.Email := 'user with spaces@domain.com'; // Пробелы в локальной части
      Fail('Should raise validation error for spaces in local part');
    except
      on EYooKassaValidationError do
        ; // Expected
    end;

    try
      User.Email := 'user@domain with spaces.com'; // Пробелы в домене
      Fail('Should raise validation error for spaces in domain');
    except
      on EYooKassaValidationError do
        ; // Expected
    end;

  finally
    User.Free;
  end;
end;

procedure TTestYooKassa.TestEmailAndPhoneValidationTogether;
var
  User: TYookassaUser;
  Receipt: TYookassaReceipt;
  JSON: TJSONObject;
begin
  // Тестируем совместное использование валидации email и phone
  User := TYookassaUser.Create;
  try
    // Валидация и нормализация обоих полей
    User.Email := 'TEST.User@EXAMPLE.COM';
    User.Phone := '+7 (900) 123-45-67';

    AssertEquals('test.user@example.com', User.Email);
    AssertEquals('79001234567', User.Phone);

    // Тест в составе чека
    Receipt := TYookassaReceipt.Create;
    try
      Receipt.Customer.Email := '  Customer@TEST.RU  ';
      Receipt.Customer.Phone := '8-900-123-45-67';

      AssertEquals('customer@test.ru', Receipt.Customer.Email);
      AssertEquals('79001234567', Receipt.Customer.Phone);

      // Проверяем что в JSON попадают нормализованные значения
      JSON := Receipt.ToJSON;
      try
        AssertEquals('customer@test.ru', JSON.Objects['customer'].Get('email', ''));
        AssertEquals('79001234567', JSON.Objects['customer'].Get('phone', ''));
      finally
        JSON.Free;
      end;

    finally
      Receipt.Free;
    end;

  finally
    User.Free;
  end;
end;

initialization
  RegisterTest(TTestYooKassa);

end.
