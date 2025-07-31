unit yookassa_api;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fphttpclient, base64;

type

  { TYookassaPayment }

  TYookassaPayment = class
  private
    FShopId: string;
    FSecretKey: string;
    FAmount: Currency;
    FCurrency: string;
    FDescription: string;
    FReturnUrl: string;
    function DoCreatePayment: string;
  public
    function BuildPaymentJSON: String;
    function ParseJSONResp(const aResp: String): String;
    property ShopId: string read FShopId write FShopId;
    property SecretKey: string read FSecretKey write FSecretKey;
    property Amount: Currency read FAmount write FAmount;
    property Currency: string read FCurrency write FCurrency;
    property Description: string read FDescription write FDescription;
    property ReturnUrl: string read FReturnUrl write FReturnUrl;
    function CreatePayment: string;
    class function CreatePayment(const aShopId, aSecretKey: string;
      aAmount: Currency; const aCurrency, aDescription, aReturnUrl: string): string;
  end;

implementation

uses
  opensslsockets
  ;

var
  _FrmtStngsJSON: TFormatSettings;

function TYookassaPayment.DoCreatePayment: string;
var
  aHttp: TFPHttpClient;
  aAuth: string;
  aRespStr: RawByteString;
begin
  Result := '';
  aHttp := TFPHttpClient.Create(nil);
  try
    aAuth := EncodeStringBase64(FShopId + ':' + FSecretKey);
    aHttp.AddHeader('Authorization', 'Basic ' + aAuth);
    aHttp.AddHeader('Content-Type', 'application/json');
    aHttp.AddHeader('Idempotence-Key', IntToHex(Random(MaxInt), 8) + IntToStr(Random(MaxInt)));
    aHTTP.RequestBody := TStringStream.Create(BuildPaymentJSON);
    try
      aRespStr := aHTTP.Post('https://api.yookassa.ru/v3/payments');
    finally
      aHTTP.RequestBody.Free;
    end;
    Result:=ParseJSONResp(aRespStr)
  finally
    aHttp.Free;
  end;
end;

function TYookassaPayment.BuildPaymentJSON: String;
var
  aJsonReq, aJsonAmount, aJsonConfirmation: TJSONObject;
begin
  aJsonReq := TJSONObject.Create;
  try
    aJsonAmount := TJSONObject.Create;
    aJsonAmount.Add('value', Format('%.2f', [FAmount], _FrmtStngsJSON));
    aJsonAmount.Add('currency', FCurrency);
    aJsonReq.Add('amount', aJsonAmount);

    aJsonReq.Add('description', FDescription);

    aJsonConfirmation := TJSONObject.Create;
    aJsonConfirmation.Add('type', 'redirect');
    aJsonConfirmation.Add('return_url', FReturnUrl);
    aJsonReq.Add('confirmation', aJsonConfirmation);

    aJsonReq.Add('capture', True);
    Result:=aJsonReq.AsJSON;
  finally
    aJsonReq.Free;
  end;
end;

function TYookassaPayment.ParseJSONResp(const aResp: String): String;
var
  aJsonResp: TJSONObject;
  aPaymentUrl: TJSONStringType;
begin
  aJsonResp := TJSONObject(GetJSON(aResp));
  try
    if aJsonResp.FindPath('confirmation.confirmation_url') <> nil then
      aPaymentUrl := aJsonResp.FindPath('confirmation.confirmation_url').AsString
    else if aJsonResp.Find('confirmation') <> nil then
      aPaymentUrl := aJsonResp.Objects['confirmation'].Get('confirmation_url', '')
    else
      aPaymentUrl := '';
    if aPaymentUrl = '' then
      raise Exception.Create('No confirmation_url in Yookassa response: ' + aResp);
    Result := aPaymentUrl;
  finally
    aJsonResp.Free;
  end;
end;

function TYookassaPayment.CreatePayment: string;
begin
  Result := DoCreatePayment;
end;

class function TYookassaPayment.CreatePayment(const aShopId, aSecretKey: string;
  aAmount: Currency; const aCurrency, aDescription, aReturnUrl: string): string;
var
  Payment: TYookassaPayment;
begin
  Payment := TYookassaPayment.Create;
  try
    Payment.FShopId := aShopId;
    Payment.FSecretKey := aSecretKey;
    Payment.FAmount := aAmount;
    Payment.FCurrency := aCurrency;
    Payment.FDescription := aDescription;
    Payment.FReturnUrl := aReturnUrl;
    Result := Payment.DoCreatePayment;
  finally
    Payment.Free;
  end;
end;

initialization
  _FrmtStngsJSON:=DefaultFormatSettings;
  _FrmtStngsJSON.DecimalSeparator:='.';

end.
