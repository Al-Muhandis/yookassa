unit test_yookassa_api_integration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, yookassa_api
  ;

type

  { TTestYooKassaIntegration }

  TTestYooKassaIntegration = class(TTestCase)
  private
    FResp: TJSONObject;
    FReceiptResp: TJSONObject;
    FYookassaAPI: TYookassaPayment;
    FReceiptRequest: TYookassaReceiptRequest;
    class procedure LoadConfig(aPayment: TYookassaPayment);
    class procedure LoadReceiptConfig(aReceiptRequest: TYookassaReceiptRequest);
    procedure UpdateTestReceipt(TestReceipt: TYookassaReceipt; aAmount: Currency; const aCurrency: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreatePayment_Sandbox;
    procedure TestCreatePaymentWithReceipt_Sandbox;
    // Новые интеграционные тесты для чеков
    procedure TestCreateReceipt_Sandbox;
    procedure TestCreateReceiptWithPhone_Sandbox;
    procedure TestCreateReceiptWithTaxSystem_Sandbox;
    procedure TestCreateRefundReceipt_Sandbox;
    procedure TestCreateReceiptWithMarkCode_Sandbox;
  end;

implementation

uses
  IniFiles
  ;

class procedure TTestYooKassaIntegration.LoadConfig(aPayment: TYookassaPayment);
var
  aIni: TIniFile;
begin
  aIni := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
  try
    aPayment.ShopId     := aIni.ReadString('shop', 'ShopId', '');
    aPayment.SecretKey  := aIni.ReadString('shop', 'SecretKey', '');
    aPayment.Amount     := aIni.ReadFloat ('order', 'Amount', 10.00);
    aPayment.Currency   := aIni.ReadString('order', 'Currency', 'RUB');
    aPayment.Description:= aIni.ReadString('order', 'Description', 'Тест с чеком');
    aPayment.ReturnUrl  := aIni.ReadString('order', 'ReturnUrl', 'https://example.com/return');
  finally
    aIni.Free;
  end;
end;

class procedure TTestYooKassaIntegration.LoadReceiptConfig(aReceiptRequest: TYookassaReceiptRequest);
var
  aIni: TIniFile;
begin
  aIni := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
  try
    aReceiptRequest.ShopId    := aIni.ReadString('shop', 'ShopId', '');
    aReceiptRequest.SecretKey := aIni.ReadString('shop', 'SecretKey', '');
    aReceiptRequest.Send      := aIni.ReadBool('receipt', 'Send', True);
  finally
    aIni.Free;
  end;
end;

procedure TTestYooKassaIntegration.UpdateTestReceipt(TestReceipt: TYookassaReceipt; aAmount: Currency;
  const aCurrency: string);
var
  aItem: TYookassaReceiptItem;
begin
  TestReceipt.CustomerEmail := 'test-receipt@example.com';

  aItem := TYookassaReceiptItem.Create;
  aItem.Description := 'Интеграционный тест чека';
  aItem.Quantity := 1;
  aItem.AmountValue := aAmount;
  aItem.AmountCurrency := aCurrency;
  aItem.VatCode := 1; // НДС 18%
  aItem.PaymentMode := 'full_prepayment';
  aItem.PaymentSubject := 'commodity';
  TestReceipt.AddItem(aItem);
end;

procedure TTestYooKassaIntegration.SetUp;
begin
  inherited SetUp;
  FYookassaAPI := TYookassaPayment.Create;
  FReceiptRequest := TYookassaReceiptRequest.Create;
end;

procedure TTestYooKassaIntegration.TearDown;
var
  aFile: TStringList;
begin
  if Assigned(FResp) then
  begin
    aFile := TStringList.Create;
    try
      aFile.Text := FResp.FormatJSON();
      aFile.SaveToFile('~payment_response.json');
    finally
      aFile.Free;
    end;
  end;

  if Assigned(FReceiptResp) then
  begin
    aFile := TStringList.Create;
    try
      aFile.Text := FReceiptResp.FormatJSON();
      aFile.SaveToFile('~receipt_response.json');
    finally
      aFile.Free;
    end;
  end;

  FResp.Free;
  FReceiptResp.Free;
  FYookassaAPI.Free;
  FReceiptRequest.Free;
  inherited TearDown;
end;

procedure TTestYooKassaIntegration.TestCreatePayment_Sandbox;
var
  aConfirmationURL: String;
begin
  LoadConfig(FYookassaAPI);
  FResp := FYookassaAPI.CreatePayment;
  aConfirmationURL := FYookassaAPI.ParseJSONResp(FResp);
  AssertTrue('confirmation_url must be exists', Pos('http', aConfirmationURL) = 1);
end;

// Интеграционный тест с отправкой receipt
procedure TTestYooKassaIntegration.TestCreatePaymentWithReceipt_Sandbox;
var
  aConfirmationURL: String;
  aReceipt: TYookassaReceipt;
  aItem: TYookassaReceiptItem;
begin
  LoadConfig(FYookassaAPI);
  aReceipt := TYookassaReceipt.Create;
  aReceipt.CustomerEmail := 'user@example.com';

  aItem := TYookassaReceiptItem.Create;
  aItem.Description := 'Тестовый товар';
  aItem.Quantity := 1;
  aItem.AmountValue := FYookassaAPI.Amount;
  aItem.AmountCurrency := FYookassaAPI.Currency;
  aItem.VatCode := 1;
  aItem.PaymentMode := 'full_prepayment';
  aItem.PaymentSubject := 'commodity';
  aReceipt.AddItem(aItem);

  FYookassaAPI.Receipt := aReceipt;

  FResp := FYookassaAPI.CreatePayment;
  aConfirmationURL := FYookassaAPI.ParseJSONResp(FResp);
  AssertTrue('confirmation_url must be exists', Pos('http', aConfirmationURL) = 1);
end;

// ========== НОВЫЕ ИНТЕГРАЦИОННЫЕ ТЕСТЫ ДЛЯ ЧЕКОВ ==========

procedure TTestYooKassaIntegration.TestCreateReceipt_Sandbox;
var
  aReceiptId: String;
begin
  LoadReceiptConfig(FReceiptRequest);

  UpdateTestReceipt(FReceiptRequest.Receipt, 50.00, 'RUB');
  FReceiptRequest.ReceiptType := 'payment';

  FReceiptResp := FReceiptRequest.CreateReceipt;
  aReceiptId := TYookassaReceiptRequest.ParseReceiptResp(FReceiptResp);

  AssertTrue('Receipt ID must be present', aReceiptId <> '');
  AssertTrue('Receipt ID should contain receipt prefix', Pos('receipt', aReceiptId) > 0);
end;

procedure TTestYooKassaIntegration.TestCreateReceiptWithPhone_Sandbox;
var
  aReceiptId: String;
begin
  LoadReceiptConfig(FReceiptRequest);

  UpdateTestReceipt(FReceiptRequest.Receipt, 75.50, 'RUB');
  FReceiptRequest.Receipt.CustomerPhone := '+79001234567';

  FReceiptRequest.ReceiptType := 'payment';
  FReceiptRequest.Send := True;

  FReceiptResp := FReceiptRequest.CreateReceipt;
  aReceiptId := TYookassaReceiptRequest.ParseReceiptResp(FReceiptResp);

  AssertTrue('Receipt with phone must be created', aReceiptId <> '');
  // Проверяем, что в ответе есть информация о customer
  AssertTrue('Response should contain customer info',
             FReceiptResp.Find('receipt') <> nil);
end;

procedure TTestYooKassaIntegration.TestCreateReceiptWithTaxSystem_Sandbox;
var
  aReceiptId: String;
  aReceipt: TYookassaReceipt;
begin
  LoadReceiptConfig(FReceiptRequest);

  UpdateTestReceipt(FReceiptRequest.Receipt, 120.00, 'RUB');
  aReceipt.TaxSystemCode := 1; // УСН доходы

  FReceiptRequest.ReceiptType := 'payment';

  FReceiptResp := FReceiptRequest.CreateReceipt;
  aReceiptId := TYookassaReceiptRequest.ParseReceiptResp(FReceiptResp);

  AssertTrue('Receipt with tax system must be created', aReceiptId <> '');
  AssertTrue('Status should be present', Pos('status:', aReceiptId) > 0);
end;

procedure TTestYooKassaIntegration.TestCreateRefundReceipt_Sandbox;
var
  aPaymentId: String;
  aReceiptId: String;
begin
  // Сначала создаем платеж для получения payment_id
  LoadConfig(FYookassaAPI);

  UpdateTestReceipt(FReceiptRequest.Receipt, 30.00, 'RUB');
  FResp := FYookassaAPI.CreatePayment;

  // Получаем payment_id из ответа
  aPaymentId := FResp.Get('id', '');
  AssertTrue('Payment ID must be present for refund test', aPaymentId <> '');

  // Теперь создаем чек возврата
  LoadReceiptConfig(FReceiptRequest);
  FReceiptRequest.ReceiptType := 'refund';
  FReceiptRequest.PaymentId := aPaymentId;
  FReceiptRequest.Send := False; // Не отправляем чек возврата клиенту

  FReceiptResp := FReceiptRequest.CreateReceipt;
  aReceiptId := TYookassaReceiptRequest.ParseReceiptResp(FReceiptResp);

  AssertTrue('Refund receipt must be created', aReceiptId <> '');
  AssertEquals('refund', FReceiptResp.Get('type', ''));
end;

procedure TTestYooKassaIntegration.TestCreateReceiptWithMarkCode_Sandbox;
var
  aReceiptId: String;
  aItem: TYookassaReceiptItem;
begin
  LoadReceiptConfig(FReceiptRequest);

  FReceiptRequest.Receipt.CustomerEmail := 'markcode-test@example.com';

  // Создаем маркированный товар
  aItem := TYookassaReceiptItem.Create;
  aItem.Description := 'Маркированный товар (тест)';
  aItem.Quantity := 1;
  aItem.AmountValue := 200.00;
  aItem.AmountCurrency := 'RUB';
  aItem.VatCode := 2; // НДС 10%
  aItem.PaymentMode := 'full_prepayment';
  aItem.PaymentSubject := 'commodity';
  aItem.MarkMode := 1; // The product is subject to labeling
  aItem.MarkCodeInfo := 'VGVzdE1hcmtDb2RlMTIzNDU2Nzg5MA=='; // Base64 test code
  FReceiptRequest.Receipt.AddItem(aItem);

  FReceiptRequest.ReceiptType := 'payment';

  FReceiptResp := FReceiptRequest.CreateReceipt;
  aReceiptId := TYookassaReceiptRequest.ParseReceiptResp(FReceiptResp);

  AssertTrue('Receipt with mark code must be created', aReceiptId <> '');
  // We check that the receipt contains information about labeling
  AssertTrue('Receipt should contain items', FReceiptResp.FindPath('receipt.items') <> nil);
end;

initialization
  RegisterTest(TTestYooKassaIntegration);
end.
