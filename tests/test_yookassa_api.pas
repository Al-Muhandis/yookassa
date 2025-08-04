unit test_yookassa_api;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, yookassa_api, fpjson
  ;

type

  { TYookassaPaymentTest }

  TYookassaPaymentTest = class(TYookassaCreatePaymentRequest)
  public
    function BuildRequestJSONTest: String;
  end;

  { TYookassaReceiptRequestTest }

  TYookassaReceiptRequestTest = class(TYookassaCreateReceiptRequest)
  public
    function BuildRequestJSON: String; override;
  end;

  { TTestableGetPaymentRequest }

  TTestableGetPaymentRequest = class(TYookassaGetPaymentRequest)
  private
    class var FTestRaw: TJSONObject;
  protected
    function DoExecute: String; override;
    function BuildRequestJSON: string; override;
    function CreateResponse(aRaw: TJSONObject): TYookassaResponse; override;
    class property TestRaw: TJSONObject read FTestRaw write FTestRaw;
  end;

  { TTestYooKassa }

  TTestYooKassa = class(TTestCase)
  private
    FPaymentRequest: TYookassaPaymentTest;
    FReceiptRequest: TYookassaReceiptRequestTest;
    FTestItem: TYookassaReceiptItem;
    procedure CallToJSON;
    procedure LogHandler(aEvent: TEventType; const aMsg: string);
    procedure CreatePaymentStaticHandler;
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
    procedure TestSupplier_InReceiptItemToJSON;
    procedure TestLogging;
    procedure TestMarkCodeInfoValidation;
  end;

implementation

uses
  yookassa_exceptions
  ;

{ TYookassaPaymentTest }

function TYookassaPaymentTest.BuildRequestJSONTest: String;
begin
  Result:=BuildRequestJSON;
end;

{ TYookassaReceiptRequestTest }

function TYookassaReceiptRequestTest.BuildRequestJSON: String;
begin
  Result:=inherited BuildRequestJSON;
end;

{ TTestableGetPaymentRequest }

function TTestableGetPaymentRequest.DoExecute: String;
begin
  if not Assigned(FTestRaw) then
    raise Exception.Create(Format('Set up json RAW for %s', [Self.ClassName]));
  Result:=FTestRaw.AsJSON;
end;

function TTestableGetPaymentRequest.BuildRequestJSON: string;
begin
  Result:=inherited BuildRequestJSON;
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

procedure TTestYooKassa.LogHandler(aEvent: TEventType; const aMsg: string);
begin
  AssertEquals('Test message', aMsg);
  AssertEquals(Ord(etDebug), Ord(aEvent));
end;

procedure TTestYooKassa.CreatePaymentStaticHandler;
begin
  TYookassaCreatePaymentRequest.CreatePayment('', '', 100, 'RUB', 'Test', 'https://return');
end;

procedure TTestYooKassa.SetUp;
begin
  FPaymentRequest := TYookassaPaymentTest.Create;
  FReceiptRequest := TYookassaReceiptRequestTest.Create;
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
  aJSON: String;
begin
  FPaymentRequest.Amount := 123.45;
  FPaymentRequest.Currency := 'RUB';
  FPaymentRequest.Description := 'Test payment';
  FPaymentRequest.ReturnUrl:='https://sample.com/';
  aJSON := FPaymentRequest.BuildRequestJSON;
  AssertTrue(Pos('"amount"', aJSON) > 0);
  AssertTrue(Pos('"value" : "123.45"', aJSON) > 0);
end;

procedure TTestYooKassa.TestCreatePaymentStatic;
begin
  AssertException(EYooKassaValidationError, @CreatePaymentStaticHandler);
end;

procedure TTestYooKassa.TestGetPaymentRequest_BuildRequestJSON;
var
  aReq: TTestableGetPaymentRequest;
begin
  aReq := TTestableGetPaymentRequest.Create;
  try
    AssertEquals('GET request should have no body', '', aReq.BuildRequestJSON);
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
        AssertEquals('succeeded', aResp.GetStatus);
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
      AssertEquals('waiting_for_capture', aResp.GetStatus);
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

    aResp := TYookassaPaymentResponse.Create(aRaw);
    try
      AssertEquals('pay_123', aResp.ID);
      AssertEquals('pending', aResp.GetStatus);
      AssertEquals('https://yookassa.ru/checkout/pay/abc', aResp.ConfirmationURL);
      AssertEquals(100.00, aResp.Amount);
    finally
      aResp.Free;
    end;
  finally
    // aRaw now owns TYookassaPaymentResponse
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
    aItem.PaymentMode := 'full_prepayment';
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
    aReceipt.CustomerEmail := 'user@example.com';
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
    aReceipt.CustomerEmail := 'user@example.com';
    aReceipt.CustomerPhone := '+79001234567';

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
      AssertEquals('+79001234567', aCustomer.Strings['phone']);
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
  aItem: TYookassaReceiptItem;
  aJSON: TJSONObject;
begin
  aReceipt := TYookassaReceipt.Create;
  try
    aReceipt.CustomerEmail := 'user@example.com';
    aReceipt.TaxSystemCode := 1; // УСН доходы

    aItem := TYookassaReceiptItem.Create;
    aItem.Description := 'Товар с налогом';
    aItem.Quantity := 1;
    aItem.AmountValue := 200.00;
    aItem.AmountCurrency := 'RUB';
    aItem.VatCode := 2;
    aReceipt.AddItem(aItem);

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
  aItem: TYookassaReceiptItem;
  aJSON: String;
  aParsedJSON: TJSONObject;
begin
  FReceiptRequest.Receipt.CustomerEmail := 'test@example.com';

  aItem := TYookassaReceiptItem.Create;
  aItem.Description := 'Тестовый товар';
  aItem.Quantity := 1;
  aItem.AmountValue := 100.00;
  aItem.AmountCurrency := 'RUB';
  aItem.VatCode := 1;
  FReceiptRequest.Receipt.AddItem(aItem);

  FReceiptRequest.ReceiptType := 'payment';
  FReceiptRequest.Send := True;

  aJSON := FReceiptRequest.BuildRequestJSON;
  aParsedJSON := TJSONObject(GetJSON(aJSON));
  try
    AssertEquals('payment', aParsedJSON.Strings['type']);
    AssertEquals(True, aParsedJSON.Booleans['send']);
    AssertTrue(aParsedJSON.Find('receipt') <> nil);
  finally
    aParsedJSON.Free;
  end;
end;

procedure TTestYooKassa.TestReceiptRequestBuildRefundJSON;
var
  aItem: TYookassaReceiptItem;
  aJSON: String;
  aParsedJSON: TJSONObject;
begin
  FReceiptRequest.Receipt.CustomerEmail := 'refund@example.com';

  aItem := TYookassaReceiptItem.Create;
  aItem.Description := 'Возврат товара';
  aItem.Quantity := 1;
  aItem.AmountValue := 150.00;
  aItem.AmountCurrency := 'RUB';
  aItem.VatCode := 2;
  FReceiptRequest.Receipt.AddItem(aItem);

  FReceiptRequest.ReceiptType := 'refund';
  FReceiptRequest.PaymentId := 'payment_123456';
  FReceiptRequest.Send := False;

  aJSON := FReceiptRequest.BuildRequestJSON;
  aParsedJSON := TJSONObject(GetJSON(aJSON));
  try
    AssertEquals('refund', aParsedJSON.Strings['type']);
    AssertEquals(False, aParsedJSON.Booleans['send']);
    AssertEquals('payment_123456', aParsedJSON.Strings['payment_id']);
    AssertTrue(aParsedJSON.Find('receipt') <> nil);
  finally
    aParsedJSON.Free;
  end;
end;

procedure TTestYooKassa.TestReceiptRequestCreate;
var
  aItem: TYookassaReceiptItem;
begin
    FReceiptRequest.Receipt.CustomerEmail := 'create@example.com';
    FReceiptRequest.Receipt.TaxSystemCode := 2;

    aItem := TYookassaReceiptItem.Create;
    aItem.Description := 'Создание чека';
    aItem.Quantity := 1;
    aItem.AmountValue := 300.00;
    aItem.AmountCurrency := 'RUB';
    aItem.VatCode := 1;
    FReceiptRequest.Receipt.AddItem(aItem);

    FReceiptRequest.ShopId := 'test_shop_id';
    FReceiptRequest.SecretKey := 'test_secret_key';
    FReceiptRequest.ReceiptType := 'payment';
    FReceiptRequest.Send := True;

    AssertEquals('test_shop_id', FReceiptRequest.ShopId);
    AssertEquals('test_secret_key', FReceiptRequest.SecretKey);
    AssertEquals('payment', FReceiptRequest.ReceiptType);
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

    aResp := TYookassaReceiptResponse.Create(aRaw);
    try
      AssertEquals('rcpt_456', aResp.ID);
      AssertEquals('succeeded', aResp.GetStatus);
      AssertEquals('pay_789', aResp.PaymentId);
    finally
      aResp.Free;
    end;
  finally
    // aRaw freed in Destroy
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
    aItem.Supplier.Phone := '+74951234567';
    aItem.Supplier.Inn := '7701234567';

    // Stream to JSON
    aJSON := aItem.ToJSON;
    try
      // Check existance of a supplier in JSON
      AssertTrue('Item must contain "supplier"', Assigned(aJSON.Find('supplier')));

      aSupplierJSON := aJSON.Objects['supplier'];
      AssertEquals('Supplier name must match', 'ООО "Ромашка"', aSupplierJSON.Get('name', ''));
      AssertEquals('Supplier phone must match', '+74951234567', aSupplierJSON.Get('phone', ''));
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

initialization
  RegisterTest(TTestYooKassa);
end.
