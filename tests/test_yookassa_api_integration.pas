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
    FPaymentRequest: TYookassaCreatePaymentRequest;
    FReceiptRequest: TYookassaCreateReceiptRequest;                                     
    class procedure LoadRequestConf(aPaymentRequest: TYookassaRequest);
    class procedure LoadCreatePaymentConf(aPaymentRequest: TYookassaCreatePaymentRequest);
    class procedure LoadCreateReceiptConf(aReceiptRequest: TYookassaCreateReceiptRequest);
    procedure UpdateTestReceipt(TestReceipt: TYookassaReceipt; aAmount: Currency; const aCurrency: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreatePayment_Sandbox;            
    procedure TestCreatePaymentAndThenRequestPaymentAfter_Sandbox;
    procedure TestCreatePaymentWithReceipt_Sandbox;
    procedure TestCreateReceipt_Sandbox;
    procedure TestCreateReceiptWithPhone_Sandbox;
    procedure TestCreateReceiptWithTaxSystem_Sandbox;
    procedure TestCreateRefundReceipt_Sandbox;
    procedure TestCreateReceiptWithMarkCode_Sandbox;
    procedure TestReceiptAfterPayment_Agent_Sandbox;
  end;

implementation

uses
  IniFiles
  ;

class procedure TTestYooKassaIntegration.LoadRequestConf(aPaymentRequest: TYookassaRequest);
var
  aIni: TIniFile;
begin
  aIni := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
  try
    aPaymentRequest.ShopId     := aIni.ReadString('shop', 'ShopId', '');
    aPaymentRequest.SecretKey  := aIni.ReadString('shop', 'SecretKey', '');
  finally
    aIni.Free;
  end;
end;

class procedure TTestYooKassaIntegration.LoadCreatePaymentConf(aPaymentRequest: TYookassaCreatePaymentRequest);
var
  aIni: TIniFile;
begin
  aIni := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
  try
    LoadRequestConf(aPaymentRequest);
    aPaymentRequest.Amount     := aIni.ReadFloat ('order', 'Amount', 10.00);
    aPaymentRequest.Currency   := aIni.ReadString('order', 'Currency', 'RUB');
    aPaymentRequest.Description:= aIni.ReadString('order', 'Description', 'Тест с чеком');
    aPaymentRequest.ReturnUrl  := aIni.ReadString('order', 'ReturnUrl', 'https://example.com/return');
  finally
    aIni.Free;
  end;
end;

class procedure TTestYooKassaIntegration.LoadCreateReceiptConf(aReceiptRequest: TYookassaCreateReceiptRequest);
var
  aIni: TIniFile;
begin
  aIni := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
  try
    LoadRequestConf(aReceiptRequest);
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
  FPaymentRequest := TYookassaCreatePaymentRequest.Create;
  FReceiptRequest := TYookassaCreateReceiptRequest.Create;
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
  LoadCreatePaymentConf(FPaymentRequest);
  FPaymentResp := FPaymentRequest.Execute as TYookassaPaymentResponse;
  AssertTrue('confirmation_url must be exists', Pos('http', FPaymentResp.ConfirmationURL) = 1);
end;

procedure TTestYooKassaIntegration.TestCreatePaymentAndThenRequestPaymentAfter_Sandbox;
var
  aGetPaymentReq: TYookassaGetPaymentRequest;
  aPayment: TYookassaPaymentResponse;
begin
  LoadCreatePaymentConf(FPaymentRequest);
  aPayment := FPaymentRequest.Execute as TYookassaPaymentResponse;
  try
    AssertTrue('confirmation_url must be exists', Pos('http', aPayment.ConfirmationURL) = 1);
    aGetPaymentReq:=TYookassaGetPaymentRequest.Create;
    try
      LoadRequestConf(aGetPaymentReq);
      aGetPaymentReq.PaymentId:=aPayment.ID;
      FPaymentResp:=aGetPaymentReq.Execute as TYookassaPaymentResponse;
      AssertFalse('id must be exists', FPaymentResp.ID.IsEmpty);
    finally
      aGetPaymentReq.Free;
    end;
  finally
    aPayment.Free;
  end;
end;

// Integration test with receipt submission
procedure TTestYooKassaIntegration.TestCreatePaymentWithReceipt_Sandbox;
var
  aReceipt: TYookassaReceipt;
  aItem: TYookassaReceiptItem;
begin
  LoadCreatePaymentConf(FPaymentRequest);
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
  LoadCreateReceiptConf(FReceiptRequest);

  UpdateTestReceipt(FReceiptRequest.Receipt, 50.00, 'RUB');
  FReceiptRequest.ReceiptType := 'payment';

  FReceiptResp := FReceiptRequest.Execute as TYookassaReceiptResponse;

  AssertTrue('Receipt ID must be present', FReceiptResp.ID <> '');
  AssertTrue('Receipt ID should contain receipt prefix', Pos('receipt', FReceiptResp.ID) > 0);
  AssertTrue('PaymentId should be present', FReceiptResp.PaymentId <> '');
end;

procedure TTestYooKassaIntegration.TestCreateReceiptWithPhone_Sandbox;
begin
  LoadCreateReceiptConf(FReceiptRequest);

  UpdateTestReceipt(FReceiptRequest.Receipt, 75.50, 'RUB');
  FReceiptRequest.Receipt.CustomerPhone := '+79001234567';

  FReceiptRequest.ReceiptType := 'payment';
  FReceiptRequest.Send := True;

  FReceiptResp := FReceiptRequest.Execute as TYookassaReceiptResponse;

  AssertTrue('Receipt with phone must be created', FReceiptResp.ID <> '');
end;

procedure TTestYooKassaIntegration.TestCreateReceiptWithTaxSystem_Sandbox;
begin
  LoadCreateReceiptConf(FReceiptRequest);

  UpdateTestReceipt(FReceiptRequest.Receipt, 120.00, 'RUB');
  FReceiptRequest.Receipt.TaxSystemCode := 1; // УСН income

  FReceiptRequest.ReceiptType := 'payment';

  FReceiptResp := FReceiptRequest.Execute as TYookassaReceiptResponse;

  AssertTrue('Receipt with tax system must be created', FReceiptResp.ID <> '');
  AssertTrue('Status should be present', Pos('status:', FReceiptResp.ID) > 0);
end;

procedure TTestYooKassaIntegration.TestCreateRefundReceipt_Sandbox;
var
  aPaymentId: String;
begin
  // First, we create a payment to receive the payment_id
  LoadCreatePaymentConf(FPaymentRequest);

  UpdateTestReceipt(FReceiptRequest.Receipt, 30.00, 'RUB');
  FPaymentResp := FPaymentRequest.Execute as TYookassaPaymentResponse;

  // Getting the payment_id from the response
  aPaymentId := FPaymentResp.ID;
  AssertTrue('Payment ID must be present for refund test', aPaymentId <> '');

  // Create refund receipt now
  LoadCreateReceiptConf(FReceiptRequest);
  FReceiptRequest.ReceiptType := 'refund';
  FReceiptRequest.PaymentId := aPaymentId;
  FReceiptRequest.Send := False; // Do not send refund receipt to client

  FReceiptResp := FReceiptRequest.Execute as TYookassaReceiptResponse;

  AssertTrue('Refund receipt must be created', FReceiptResp.ID <> '');
  AssertEquals('refund', FReceiptResp.Raw.Get('type', ''));
end;

procedure TTestYooKassaIntegration.TestCreateReceiptWithMarkCode_Sandbox;
var
  aItem: TYookassaReceiptItem;
begin
  LoadCreateReceiptConf(FReceiptRequest);

  FReceiptRequest.Receipt.CustomerEmail := 'markcode-test@example.com';

  // Creating a labeled product
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

  AssertTrue('Receipt with mark code must be created', FReceiptResp.ID <> '');
  // We check that the receipt contains information about labeling
  AssertTrue('Receipt should contain items', FReceiptResp.Raw.FindPath('receipt.items') <> nil);
end;

procedure TTestYooKassaIntegration.TestReceiptAfterPayment_Agent_Sandbox;
var
  aPayment: TYookassaCreatePaymentRequest;
  aReceiptReq: TYookassaCreateReceiptRequest;
  aItem: TYookassaReceiptItem;
  aRawItem: TJSONData;
  aPaymentID: String;
begin
  // Step 1: create payment
  aPayment := TYookassaCreatePaymentRequest.Create;
  try
    LoadCreatePaymentConf(aPayment);
    FPaymentResp := aPayment.Execute as TYookassaPaymentResponse;
    aPaymentID:=FPaymentResp.ID;
    AssertTrue('Payment ID must be present', aPaymentID <> '');
  finally
    aPayment.Free;
  end;

  // Step 2: Create a receipt indicating the supplier (agent scheme)
  aReceiptReq := TYookassaCreateReceiptRequest.Create;
  try
    LoadCreateReceiptConf(aReceiptReq);
    aReceiptReq.Receipt.CustomerEmail := 'agent-client@example.com';
    aReceiptReq.PaymentId := aPaymentID; // required field
    aReceiptReq.ReceiptType := 'payment';
    aReceiptReq.Send := True;

    // Adding a product with a supplier
    aItem := TYookassaReceiptItem.Create;
    try
      aItem.Description := 'Товар от самозанятого';
      aItem.Quantity := 1.0;
      aItem.AmountValue := 500.00;
      aItem.AmountCurrency := 'RUB';
      aItem.VatCode := 1; // НДС 18%
      aItem.PaymentMode := 'full_prepayment';
      aItem.PaymentSubject := 'commodity';

      // supplier
      aItem.Supplier.Name := 'Иванов И.П.';
      aItem.Supplier.Phone := '+79001234567';
      aItem.Supplier.Inn := '123456789012';

      aReceiptReq.Receipt.AddItem(aItem);

      // send receipt
      FReceiptResp := aReceiptReq.Execute as TYookassaReceiptResponse;

      // check response
      AssertTrue('Receipt must be created', FReceiptResp.ID <> '');
      AssertEquals('succeeded', FReceiptResp.GetStatus);
      AssertEquals(aPaymentId, FReceiptResp.PaymentId);

      // check that supplier was sent
      aRawItem := FReceiptResp.Raw.FindPath('receipt.items[0]');
      AssertTrue('Supplier must be in receipt item', Assigned(aRawItem));
      AssertTrue('Supplier must be present', Assigned(aRawItem.FindPath('supplier')));
      AssertEquals('Иванов И.П.', (aRawItem as TJSONObject).Objects['supplier'].Get('name', ''));
    finally
      aItem.Free;
    end;
  finally
    aReceiptReq.Free;
  end;
end;

initialization
  RegisterTest(TTestYooKassaIntegration);
end.
