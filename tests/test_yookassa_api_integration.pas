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
    FPaymentResp: TYookassaPaymentResponse;
    FReceiptResp: TYookassaReceiptResponse;
    FPaymentRequest: TYookassaPaymentRequest;
    FReceiptRequest: TYookassaReceiptRequest;
    class procedure LoadPaymentConfig(aPaymentRequest: TYookassaPaymentRequest);
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

class procedure TTestYooKassaIntegration.LoadPaymentConfig(aPaymentRequest: TYookassaPaymentRequest);
var
  aIni: TIniFile;
begin
  aIni := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
  try
    aPaymentRequest.ShopId     := aIni.ReadString('shop', 'ShopId', '');
    aPaymentRequest.SecretKey  := aIni.ReadString('shop', 'SecretKey', '');
    aPaymentRequest.Amount     := aIni.ReadFloat ('order', 'Amount', 10.00);
    aPaymentRequest.Currency   := aIni.ReadString('order', 'Currency', 'RUB');
    aPaymentRequest.Description:= aIni.ReadString('order', 'Description', 'Тест с чеком');
    aPaymentRequest.ReturnUrl  := aIni.ReadString('order', 'ReturnUrl', 'https://example.com/return');
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
  FPaymentRequest := TYookassaPaymentRequest.Create;
  FReceiptRequest := TYookassaReceiptRequest.Create;
  FPaymentResp := nil;
  FReceiptResp := nil;
end;

procedure TTestYooKassaIntegration.TearDown;
var
  aFile: TStringList;
begin
  if Assigned(FPaymentResp) then
  begin
    aFile := TStringList.Create;
    try
      aFile.Text := FPaymentResp.Raw.FormatJSON();
      aFile.SaveToFile('~payment_response.json');
    finally
      aFile.Free;
    end;
  end;

  if Assigned(FReceiptResp) then
  begin
    aFile := TStringList.Create;
    try
      aFile.Text := FReceiptResp.Raw.FormatJSON();
      aFile.SaveToFile('~receipt_response.json');
    finally
      aFile.Free;
    end;
  end;

  FPaymentResp.Free;
  FReceiptResp.Free;
  FPaymentRequest.Free;
  FReceiptRequest.Free;
  inherited TearDown;
end;

procedure TTestYooKassaIntegration.TestCreatePayment_Sandbox;
begin
  LoadPaymentConfig(FPaymentRequest);
  FPaymentResp := FPaymentRequest.Execute as TYookassaPaymentResponse;
  AssertTrue('confirmation_url must be exists', Pos('http', FPaymentResp.ConfirmationURL) = 1);
end;

// Интеграционный тест с отправкой receipt
procedure TTestYooKassaIntegration.TestCreatePaymentWithReceipt_Sandbox;
var
  aReceipt: TYookassaReceipt;
  aItem: TYookassaReceiptItem;
begin
  LoadPaymentConfig(FPaymentRequest);
  aReceipt := TYookassaReceipt.Create;
  aReceipt.CustomerEmail := 'user@example.com';

  aItem := TYookassaReceiptItem.Create;
  aItem.Description := 'Тестовый товар';
  aItem.Quantity := 1;
  aItem.AmountValue := FPaymentRequest.Amount;
  aItem.AmountCurrency := FPaymentRequest.Currency;
  aItem.VatCode := 1;
  aItem.PaymentMode := 'full_prepayment';
  aItem.PaymentSubject := 'commodity';
  aReceipt.AddItem(aItem);

  FPaymentRequest.Receipt := aReceipt;

  FPaymentResp := FPaymentRequest.Execute as TYookassaPaymentResponse;
  AssertTrue('confirmation_url must be exists', Pos('http', FPaymentResp.ConfirmationURL) = 1);
end;

procedure TTestYooKassaIntegration.TestCreateReceipt_Sandbox;
begin
  LoadReceiptConfig(FReceiptRequest);

  UpdateTestReceipt(FReceiptRequest.Receipt, 50.00, 'RUB');
  FReceiptRequest.ReceiptType := 'payment';

  FReceiptResp := FReceiptRequest.Execute as TYookassaReceiptResponse;

  AssertTrue('Receipt ID must be present', FReceiptResp.GetId <> '');
  AssertTrue('Receipt ID should contain receipt prefix', Pos('receipt', FReceiptResp.GetId) > 0);
  AssertTrue('PaymentId should be present', FReceiptResp.PaymentId <> '');
end;

procedure TTestYooKassaIntegration.TestCreateReceiptWithPhone_Sandbox;
begin
  LoadReceiptConfig(FReceiptRequest);

  UpdateTestReceipt(FReceiptRequest.Receipt, 75.50, 'RUB');
  FReceiptRequest.Receipt.CustomerPhone := '+79001234567';

  FReceiptRequest.ReceiptType := 'payment';
  FReceiptRequest.Send := True;

  FReceiptResp := FReceiptRequest.Execute as TYookassaReceiptResponse;

  AssertTrue('Receipt with phone must be created', FReceiptResp.GetId <> '');
end;

procedure TTestYooKassaIntegration.TestCreateReceiptWithTaxSystem_Sandbox;
begin
  LoadReceiptConfig(FReceiptRequest);

  UpdateTestReceipt(FReceiptRequest.Receipt, 120.00, 'RUB');
  FReceiptRequest.Receipt.TaxSystemCode := 1; // УСН income

  FReceiptRequest.ReceiptType := 'payment';

  FReceiptResp := FReceiptRequest.Execute as TYookassaReceiptResponse;

  AssertTrue('Receipt with tax system must be created', FReceiptResp.GetId <> '');
  AssertTrue('Status should be present', Pos('status:', FReceiptResp.GetId) > 0);
end;

procedure TTestYooKassaIntegration.TestCreateRefundReceipt_Sandbox;
var
  aPaymentId: String;
begin
  // First, we create a payment to receive the payment_id
  LoadPaymentConfig(FPaymentRequest);

  UpdateTestReceipt(FReceiptRequest.Receipt, 30.00, 'RUB');
  FPaymentResp := FPaymentRequest.Execute as TYookassaPaymentResponse;

  // Getting the payment_id from the response
  aPaymentId := FPaymentResp.GetId;
  AssertTrue('Payment ID must be present for refund test', aPaymentId <> '');

  // Create refund receipt now
  LoadReceiptConfig(FReceiptRequest);
  FReceiptRequest.ReceiptType := 'refund';
  FReceiptRequest.PaymentId := aPaymentId;
  FReceiptRequest.Send := False; // Do not send refund receipt to client

  FReceiptResp := FReceiptRequest.Execute as TYookassaReceiptResponse;

  AssertTrue('Refund receipt must be created', FReceiptResp.GetId <> '');
  AssertEquals('refund', FReceiptResp.Raw.Get('type', ''));
end;

procedure TTestYooKassaIntegration.TestCreateReceiptWithMarkCode_Sandbox;
var
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

  FReceiptResp := FReceiptRequest.Execute as TYookassaReceiptResponse;

  AssertTrue('Receipt with mark code must be created', FReceiptResp.GetId <> '');
  // We check that the receipt contains information about labeling
  AssertTrue('Receipt should contain items', FReceiptResp.Raw.FindPath('receipt.items') <> nil);
end;

initialization
  RegisterTest(TTestYooKassaIntegration);
end.
