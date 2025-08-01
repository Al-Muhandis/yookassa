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
    FYookassaAPI: TYookassaPayment;
    class procedure LoadConfig(aPayment: TYookassaPayment);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreatePayment_Sandbox;
    procedure TestCreatePaymentWithReceipt_Sandbox;
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

procedure TTestYooKassaIntegration.SetUp;
begin
  inherited SetUp;
  FYookassaAPI:=TYookassaPayment.Create;
end;

procedure TTestYooKassaIntegration.TearDown;
var
  aFile: TStringList;
begin
  if Assigned(FResp) then
  begin
    aFile:=TStringList.Create;
    try
      aFile.Text:=FResp.FormatJSON();
      aFile.SaveToFile('~response.json');
    finally              
      aFile.Free;
    end;
  end;
  FResp.Free;
  FYookassaAPI.Free;
  inherited TearDown;
end;

procedure TTestYooKassaIntegration.TestCreatePayment_Sandbox;
var
  aConfirmationURL: String;
begin
  LoadConfig(FYookassaAPI);
  FResp := FYookassaAPI.CreatePayment;
  aConfirmationURL:=FYookassaAPI.ParseJSONResp(FResp);
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
  aConfirmationURL:=FYookassaAPI.ParseJSONResp(FResp);
  AssertTrue('confirmation_url must be exists', Pos('http', aConfirmationURL) = 1);
end;

initialization
  RegisterTest(TTestYooKassaIntegration);
end.
