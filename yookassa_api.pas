unit yookassa_api;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fphttpclient, base64;

type
  { TYookassaReceiptItem }
  TYookassaReceiptItem = class
  public
    Description: string;
    Quantity: Double;
    AmountValue: Currency;
    AmountCurrency: string;
    VatCode: Integer;
    PaymentMode: string;
    PaymentSubject: string;
    MarkMode: Integer;
    MarkCodeInfo: string; // base64 gs_1m,
    Measure: string;
    constructor Create;
    function ToJSON: TJSONObject;
  end;

  { TYookassaReceipt }
  TYookassaReceipt = class
  private
    FCustomerEmail: string;
    FItems: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddItem(aItem: TYookassaReceiptItem);
    function ToJSON: TJSONObject;
    property CustomerEmail: String read FCustomerEmail write FCustomerEmail;
    property Items: TList read FItems write FItems;
  end;

  { TYookassaPayment }
  TYookassaPayment = class
  private
    FShopId: string;
    FSecretKey: string;
    FAmount: Currency;
    FCurrency: string;
    FDescription: string;
    FReturnUrl: string;

    FReceipt: TYookassaReceipt;
    FMetaOrderId: string;
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
    property Receipt: TYookassaReceipt read FReceipt write FReceipt;
    property MetaOrderId: string read FMetaOrderId write FMetaOrderId; // Для metadata.order_id
    function CreatePayment: string;
    class function CreatePayment(const aShopId, aSecretKey: string;
      aAmount: Currency; const aCurrency, aDescription, aReturnUrl: string): string;
    destructor Destroy; override;
  end;

implementation

uses
  opensslsockets
  ;

var
  _FrmtStngsJSON: TFormatSettings;

{ TYookassaReceiptItem }

constructor TYookassaReceiptItem.Create;
begin
  MarkMode := -1; // -1 = не задан
end;

function TYookassaReceiptItem.ToJSON: TJSONObject;
var
  aAmount: TJSONObject;
  aMarkCodeInfo: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('description', Description);
  Result.Add('quantity', Quantity);
  aAmount := TJSONObject.Create;
  aAmount.Add('value', Format('%.2f', [AmountValue], _FrmtStngsJSON));
  aAmount.Add('currency', AmountCurrency);
  Result.Add('amount', aAmount);
  Result.Add('vat_code', VatCode);
  if PaymentMode <> '' then Result.Add('payment_mode', PaymentMode);
  if PaymentSubject <> '' then Result.Add('payment_subject', PaymentSubject);
  if MarkMode >= 0 then Result.Add('mark_mode', MarkMode);
  if MarkCodeInfo <> '' then begin
    aMarkCodeInfo := TJSONObject.Create;
    aMarkCodeInfo.Add('gs_1m', MarkCodeInfo);
    Result.Add('mark_code_info', aMarkCodeInfo);
  end;
  if Measure <> '' then Result.Add('measure', Measure);
end;

{ TYookassaReceipt }

constructor TYookassaReceipt.Create;
begin
  Items := TList.Create;
end;

destructor TYookassaReceipt.Destroy;
var
  i: Integer;
begin
  for i := 0 to Items.Count - 1 do
    TObject(Items[i]).Free;
  FItems.Free;
  inherited Destroy;
end;

procedure TYookassaReceipt.AddItem(aItem: TYookassaReceiptItem);
begin
  Items.Add(aItem);
end;

function TYookassaReceipt.ToJSON: TJSONObject;
var
  aJson: TJSONObject;
  aItems: TJSONArray;
  i: Integer;
begin
  aJson := TJSONObject.Create;
  if CustomerEmail <> '' then begin
    aJson.Add('customer', TJSONObject.Create(['email', CustomerEmail]));
  end;
  aItems := TJSONArray.Create;
  for i := 0 to Items.Count - 1 do
    aItems.Add(TYookassaReceiptItem(Items[i]).ToJSON);
  aJson.Add('items', aItems);
  Result := aJson;
end;

{ TYookassaPayment }

destructor TYookassaPayment.Destroy;
begin
  FReceipt.Free;
  inherited Destroy;
end;

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
  aJsonReq, aJsonAmount, aJsonConfirmation, aJsonMeta: TJSONObject;
begin
  aJsonReq := TJSONObject.Create;
  try
    // amount
    aJsonAmount := TJSONObject.Create;
    aJsonAmount.Add('value', Format('%.2f', [FAmount], _FrmtStngsJSON));
    aJsonAmount.Add('currency', FCurrency);
    aJsonReq.Add('amount', aJsonAmount);

    aJsonReq.Add('description', FDescription);

    // confirmation
    aJsonConfirmation := TJSONObject.Create;
    aJsonConfirmation.Add('type', 'redirect');
    aJsonConfirmation.Add('return_url', FReturnUrl);
    aJsonReq.Add('confirmation', aJsonConfirmation);

    aJsonReq.Add('capture', True);

    // metadata
    if FMetaOrderId <> '' then
    begin
      aJsonMeta := TJSONObject.Create;
      aJsonMeta.Add('order_id', FMetaOrderId);
      aJsonReq.Add('metadata', aJsonMeta);
    end;

    // receipt
    if Assigned(FReceipt) then
      aJsonReq.Add('receipt', FReceipt.ToJSON);

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
  Randomize;

end.
