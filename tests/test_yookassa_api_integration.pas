unit test_yookassa_api_integration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, yookassa_api
  ;

type

  { TTestYooKassaIntegration }

  TTestYooKassaIntegration = class(TTestCase)
  private
    procedure LoadConfig(aPayment: TYookassaPayment);
  published
    procedure TestCreatePayment_Sandbox;
    procedure TestCreatePaymentWithReceipt_Sandbox;
  end;

implementation

uses
  IniFiles
  ;

procedure TTestYooKassaIntegration.LoadConfig(aPayment: TYookassaPayment);
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

procedure TTestYooKassaIntegration.TestCreatePayment_Sandbox;
var
  aConfirmationURL: String;

  procedure CreatePayment;
  var
    aPayment: TYookassaPayment;
  begin
    aPayment := TYookassaPayment.Create;
    try
      LoadConfig(aPayment);
      aConfirmationURL := aPayment.CreatePayment;
    finally
      aPayment.Free;
    end;
  end;

begin
  CreatePayment;
  AssertTrue('confirmation_url must be exists', Pos('http', aConfirmationURL) = 1);
end;

// Интеграционный тест с отправкой receipt
procedure TTestYooKassaIntegration.TestCreatePaymentWithReceipt_Sandbox;
var
  aConfirmationURL: String;

  procedure CreatePaymentWithReceipt;
  var
    aPayment: TYookassaPayment;
    aReceipt: TYookassaReceipt;
    aItem: TYookassaReceiptItem;
  begin
    aPayment := TYookassaPayment.Create;
    try
      LoadConfig(aPayment);
      aReceipt := TYookassaReceipt.Create;
      aReceipt.CustomerEmail := 'user@example.com';

      aItem := TYookassaReceiptItem.Create;
      aItem.Description := 'Тестовый товар';
      aItem.Quantity := 1;
      aItem.AmountValue := aPayment.Amount;
      aItem.AmountCurrency := aPayment.Currency;
      aItem.VatCode := 1;
      aItem.PaymentMode := 'full_prepayment';
      aItem.PaymentSubject := 'commodity';
      aReceipt.AddItem(aItem);

      aPayment.Receipt := aReceipt;

      aConfirmationURL := aPayment.CreatePayment;

    finally
      aPayment.Free;
    end;
  end;

begin
  CreatePaymentWithReceipt;
  AssertTrue('confirmation_url must be exists', Pos('http', aConfirmationURL) = 1);
end;

initialization
  RegisterTest(TTestYooKassaIntegration);
end.
