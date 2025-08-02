unit test_yookassa_api;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, yookassa_api, fpjson
  ;

type

  { TYookassaPaymentTest }

  TYookassaPaymentTest = class(TYookassaPaymentRequest)
  public
    function BuildRequestJSONTest: String;
  end;

  { TYookassaReceiptRequestTest }

  TYookassaReceiptRequestTest = class(TYookassaReceiptRequest)
  public
    function BuildRequestJSONTest: String;
    function ParseResponse(const AResponse: String): TJSONObject; override;
  end;

  { TTestYooKassa }

  TTestYooKassa = class(TTestCase)
  private
    FYookassaAPI: TYookassaPaymentTest;
    FReceiptRequest: TYookassaReceiptRequestTest;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBuildRequestData;
    procedure TestReceiptItemToJSON;
    procedure TestReceiptToJSON;
    procedure TestReceiptWithPhoneToJSON;
    procedure TestReceiptWithTaxSystemCodeToJSON;
    procedure TestReceiptRequestBuildJSON;
    procedure TestReceiptRequestBuildRefundJSON;
    procedure TestReceiptRequestParseSuccessResponse;
    procedure TestReceiptRequestCreate;
    procedure TestReceiptItemMarkCodeInfo;
    procedure TestReceiptEmptyCustomer;
  end;

implementation

{ TYookassaPaymentTest }

function TYookassaPaymentTest.BuildRequestJSONTest: String;
begin
  Result:=BuildRequestJSON;
end;

{ TYookassaReceiptRequestTest }

function TYookassaReceiptRequestTest.BuildRequestJSONTest: String;
begin
  Result:=BuildRequestJSON;
end;

function TYookassaReceiptRequestTest.ParseResponse(const AResponse: String): TJSONObject;
begin
  Result:=inherited ParseResponse(AResponse);
end;

procedure TTestYooKassa.SetUp;
begin
  FYookassaAPI := TYookassaPaymentTest.Create;
  FReceiptRequest := TYookassaReceiptRequestTest.Create;
  inherited SetUp;
end;

procedure TTestYooKassa.TearDown;
begin
  FYookassaAPI.Free;
  FReceiptRequest.Free;
  inherited TearDown;
end;

procedure TTestYooKassa.TestBuildRequestData;
var
  aJSON: String;
begin
  FYookassaAPI.Amount := 123.45;
  FYookassaAPI.Currency := 'RUB';
  FYookassaAPI.Description := 'Test payment';
  FYookassaAPI.ReturnUrl:='https://sample.com/';
  aJSON := FYookassaAPI.BuildRequestJSON;
  AssertTrue(Pos('"amount"', aJSON) > 0);
  AssertTrue(Pos('"value" : "123.45"', aJSON) > 0);
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
    aItem.MarkMode := 0;
    aItem.MarkCodeInfo := 'Base64Code';
    aItem.Measure := 'piece';

    aJSON := aItem.ToJSON;
    try
      AssertEquals('Товар', aJSON.Strings['description']);
      AssertEquals(2.0, aJSON.Floats['quantity']);
      AssertTrue(aJSON.Find('amount') <> nil);
      AssertEquals('RUB', TJSONObject(aJSON.Objects['amount']).Strings['currency']);
      AssertEquals('full_prepayment', aJSON.Strings['payment_mode']);
      AssertEquals('commodity', aJSON.Strings['payment_subject']);
      AssertEquals(0, aJSON.Integers['mark_mode']);
      AssertTrue(aJSON.Find('mark_code_info') <> nil);
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

// ========== НОВЫЕ ТЕСТЫ ДЛЯ ФУНКЦИОНАЛА ЧЕКОВ ==========

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

  aJSON := FReceiptRequest.BuildRequestJSONTest;
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

  aJSON := FReceiptRequest.BuildRequestJSONTest;
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

procedure TTestYooKassa.TestReceiptRequestParseSuccessResponse;
var
  aJSON: String;
  aResp: TJSONObject;
begin
  aJSON := '{"id":"receipt_123","status":"succeeded","type":"payment","send":true}';
  try
    aResp := FReceiptRequest.ParseResponse(aJSON);
    AssertTrue(FReceiptRequest.ReceiptID='receipt_123');
    AssertTrue('succeeded'=aResp.Strings['status']);
  finally
    aResp.Free;
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

procedure TTestYooKassa.TestReceiptItemMarkCodeInfo;
var
  aItem: TYookassaReceiptItem;
  aJSON: TJSONObject;
  aMarkCodeInfo: TJSONObject;
begin
  aItem := TYookassaReceiptItem.Create;
  try
    aItem.Description := 'Маркированный товар';
    aItem.Quantity := 1.0;
    aItem.AmountValue := 500.00;
    aItem.AmountCurrency := 'RUB';
    aItem.VatCode := 1;
    aItem.MarkMode := 1;
    aItem.MarkCodeInfo := 'BASE64_ENCODED_MARK_CODE';

    aJSON := aItem.ToJSON;
    try
      AssertEquals(1, aJSON.Integers['mark_mode']);
      AssertTrue(aJSON.Find('mark_code_info') <> nil);
      aMarkCodeInfo := TJSONObject(aJSON.Objects['mark_code_info']);
      AssertEquals('BASE64_ENCODED_MARK_CODE', aMarkCodeInfo.Strings['gs_1m']);
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

initialization
  RegisterTest(TTestYooKassa);
end.
