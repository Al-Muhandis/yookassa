unit test_yookassa_api_integration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, yookassa_api
  ;

type
  TTestYooKassaIntegration = class(TTestCase)
  published
    procedure TestCreatePayment_Sandbox;
  end;

implementation

uses
  IniFiles
  ;

procedure TTestYooKassaIntegration.TestCreatePayment_Sandbox;
var
  aShopId, aSecretKey, aCurrency, aDescription, aReturnUrl: string;
  aAmount: Double;
  aConfirmationURL: String;

  procedure LoadConfig;
  var
    aIni: TIniFile;
  begin
    aIni := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
    try
      aShopId     := aIni.ReadString('shop', 'ShopId', '');
      aSecretKey  := aIni.ReadString('shop', 'SecretKey', '');
      aAmount     := aIni.ReadFloat ('order', 'Amount', 10.00);
      aCurrency   := aIni.ReadString('order', 'Currency', 'RUB');
      aDescription:= aIni.ReadString('order', 'Description', 'Тест');
      aReturnUrl  := aIni.ReadString('order', 'ReturnUrl', 'https://example.com/return');
    finally
      aIni.Free;
    end;
  end;

  procedure CreatePayment;
  var
    aPayment: TYookassaPayment;
  begin
    aPayment := TYookassaPayment.Create;
    try
      aPayment.ShopId     := aShopId;
      aPayment.SecretKey  := aSecretKey;
      aPayment.Amount     := aAmount;
      aPayment.Currency   := aCurrency;
      aPayment.Description:= aDescription;
      aPayment.ReturnUrl  := aReturnUrl;

      aConfirmationURL := aPayment.CreatePayment;
    finally
      aPayment.Free;
    end;
  end;

begin
  LoadConfig;
  CreatePayment;
  AssertTrue('confirmation_url must be exists', Pos('http', aConfirmationURL) = 1);
end;

initialization
  RegisterTest(TTestYooKassaIntegration);
end.
