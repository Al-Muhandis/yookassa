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
    FCustomerPhone: string;
    FItems: TList;
    FTaxSystemCode: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddItem(aItem: TYookassaReceiptItem);
    function ToJSON: TJSONObject;
    function ToReceiptJSON: TJSONObject; // to create a receipt separately
    property CustomerEmail: String read FCustomerEmail write FCustomerEmail;
    property CustomerPhone: String read FCustomerPhone write FCustomerPhone;
    property Items: TList read FItems write FItems;
    property TaxSystemCode: Integer read FTaxSystemCode write FTaxSystemCode;
  end;

  { TYookassaReceiptRequest }
  TYookassaReceiptRequest = class
  private
    FShopId: string;
    FSecretKey: string;
    FReceiptType: string; // 'payment', 'refund'
    FPaymentId: string; // for the refund receipt
    FRefundId: string; // for the refund receipt
    FReceipt: TYookassaReceipt;
    FSend: Boolean;
    FSettlements: TJSONArray; // to split payments
    function DoCreateReceipt: TJSONObject;
    function GetReceipt: TYookassaReceipt;
  public
    constructor Create;
    destructor Destroy; override;
    function BuildReceiptJSON: String;
    function CreateReceipt: TJSONObject;
    class function CreateReceipt(const aShopId, aSecretKey: string;
      aReceipt: TYookassaReceipt; const aReceiptType: string = 'payment';
      const aPaymentId: string = ''; aSend: Boolean = True): TJSONObject;
    class function ParseReceiptResp(const aResp: TJSONObject): String;
    property ShopId: string read FShopId write FShopId;
    property SecretKey: string read FSecretKey write FSecretKey;
    property ReceiptType: string read FReceiptType write FReceiptType;
    property PaymentId: string read FPaymentId write FPaymentId;
    property RefundId: string read FRefundId write FRefundId;
    property Receipt: TYookassaReceipt read GetReceipt;
    property Send: Boolean read FSend write FSend;
    property Settlements: TJSONArray read FSettlements write FSettlements;
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
    function DoCreatePayment: TJSONObject;
  public
    function BuildPaymentJSON: String;
    function CreatePayment: TJSONObject;
    class function CreatePayment(const aShopId, aSecretKey: string;
      aAmount: Currency; const aCurrency, aDescription, aReturnUrl: string): string;
    destructor Destroy; override;
    class function ParseJSONResp(const aResp: TJSONObject): String;
    property ShopId: string read FShopId write FShopId;
    property SecretKey: string read FSecretKey write FSecretKey;
    property Amount: Currency read FAmount write FAmount;
    property Currency: string read FCurrency write FCurrency;
    property Description: string read FDescription write FDescription;
    property ReturnUrl: string read FReturnUrl write FReturnUrl;
    property Receipt: TYookassaReceipt read FReceipt write FReceipt;
    property MetaOrderId: string read FMetaOrderId write FMetaOrderId;
  end;

implementation

uses
  opensslsockets, yookassa_exceptions
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
  FTaxSystemCode := -1; // not specified
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
  aCustomer: TJSONObject;
  i: Integer;
begin
  aJson := TJSONObject.Create;
  
  // customer
  if (CustomerEmail <> '') or (CustomerPhone <> '') then begin
    aCustomer := TJSONObject.Create;
    if CustomerEmail <> '' then aCustomer.Add('email', CustomerEmail);
    if CustomerPhone <> '' then aCustomer.Add('phone', CustomerPhone);
    aJson.Add('customer', aCustomer);
  end;
  
  // items
  aItems := TJSONArray.Create;
  for i := 0 to Items.Count - 1 do
    aItems.Add(TYookassaReceiptItem(Items[i]).ToJSON);
  aJson.Add('items', aItems);
  
  // tax_system_code
  if FTaxSystemCode >= 0 then
    aJson.Add('tax_system_code', FTaxSystemCode);
  
  Result := aJson;
end;

function TYookassaReceipt.ToReceiptJSON: TJSONObject;
begin
  Result := ToJSON;
end;

{ TYookassaReceiptRequest }

constructor TYookassaReceiptRequest.Create;
begin
  FSend := True; // by default, we send the receipt to the client
  FReceiptType := 'payment'; // by default, the payment receipt
end;

destructor TYookassaReceiptRequest.Destroy;
begin
  FSettlements.Free;
  FReceipt.Free;
  inherited Destroy;
end;

function TYookassaReceiptRequest.DoCreateReceipt: TJSONObject;
var
  aHttp: TFPHttpClient;
  aAuth: string;
  aRespStr: RawByteString;
  ErrJSON: TJSONObject;
begin
  Result := nil;
  aHttp := TFPHttpClient.Create(nil);
  try
    aAuth := EncodeStringBase64(FShopId + ':' + FSecretKey);
    aHttp.AddHeader('Authorization', 'Basic ' + aAuth);
    aHttp.AddHeader('Content-Type', 'application/json');
    aHttp.AddHeader('Idempotence-Key', IntToHex(Random(MaxInt), 8) + IntToStr(Random(MaxInt)));

    aHTTP.RequestBody := TStringStream.Create(BuildReceiptJSON);
    try
      aRespStr := aHTTP.Post('https://api.yookassa.ru/v3/receipts');
    finally
      aHTTP.RequestBody.Free;
    end;

    // Check status
    if aHTTP.ResponseStatusCode >= 400 then
    begin
      try
        ErrJSON := TJSONObject(GetJSON(aRespStr));
      except
        on E: Exception do
          raise EYooKassaError.CreateFmt('Invalid error response from YooKassa: %s', [aRespStr]);
      end;
      raise EYooKassaError.CreateFromResponse(aHTTP.ResponseStatusCode, ErrJSON);
    end;

    Result := TJSONObject(GetJSON(aRespStr));
  finally
    aHttp.Free;
  end;
end;

function TYookassaReceiptRequest.GetReceipt: TYookassaReceipt;
begin
  if not Assigned(FReceipt) then
    FReceipt:=TYookassaReceipt.Create;
  Result:=FReceipt;
end;

function TYookassaReceiptRequest.BuildReceiptJSON: String;
var
  aJsonReq: TJSONObject;
begin
  aJsonReq := TJSONObject.Create;
  try
    // тип чека (payment/refund)
    aJsonReq.Add('type', FReceiptType);
    
    // отправлять ли чек покупателю
    aJsonReq.Add('send', FSend);
    
    // данные чека
    if Assigned(FReceipt) then
      aJsonReq.Add('receipt', FReceipt.ToReceiptJSON);
    
    // для чека возврата нужен payment_id или refund_id
    if FReceiptType = 'refund' then begin
      if FPaymentId <> '' then
        aJsonReq.Add('payment_id', FPaymentId)
      else if FRefundId <> '' then
        aJsonReq.Add('refund_id', FRefundId);
    end;
    
    // settlements для разделения платежей (если используется)
    if Assigned(FSettlements) and (FSettlements.Count > 0) then
      aJsonReq.Add('settlements', FSettlements);
    
    Result := aJsonReq.AsJSON;
  finally
    aJsonReq.Free;
  end;
end;

function TYookassaReceiptRequest.CreateReceipt: TJSONObject;
begin
  Result := DoCreateReceipt;
end;

class function TYookassaReceiptRequest.CreateReceipt(const aShopId, aSecretKey: string;
  aReceipt: TYookassaReceipt; const aReceiptType: string = 'payment';
  const aPaymentId: string = ''; aSend: Boolean = True): TJSONObject;
var
  aReceiptReq: TYookassaReceiptRequest;
begin
  aReceiptReq := TYookassaReceiptRequest.Create;
  try
    aReceiptReq.FShopId := aShopId;
    aReceiptReq.FSecretKey := aSecretKey;
    aReceiptReq.FReceipt := aReceipt;
    aReceiptReq.FReceiptType := aReceiptType;
    aReceiptReq.FPaymentId := aPaymentId;
    aReceiptReq.FSend := aSend;
    Result := aReceiptReq.DoCreateReceipt;
  finally
    aReceiptReq.Free;
  end;
end;

class function TYookassaReceiptRequest.ParseReceiptResp(const aResp: TJSONObject): String;
var
  aReceiptId: TJSONStringType;
  aStatus: TJSONStringType;
begin
  // получаем ID чека
  aReceiptId := aResp.Get('id', '');
  aStatus := aResp.Get('status', '');
  
  if aReceiptId = '' then
    raise Exception.Create('No receipt id in YooKassa response: ' + aResp.AsJSON);
    
  Result := aReceiptId;
  
  // можно также вернуть дополнительную информацию о статусе
  if aStatus <> '' then
    Result := Result + ' (status: ' + aStatus + ')';
end;

{ TYookassaPayment }

destructor TYookassaPayment.Destroy;
begin
  FReceipt.Free;
  inherited Destroy;
end;

function TYookassaPayment.DoCreatePayment: TJSONObject;
var
  aHttp: TFPHttpClient;
  aAuth: string;
  aRespStr: RawByteString;
  ErrJSON: TJSONObject;
begin
  Result := nil;
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

    // Проверяем статус
    if aHTTP.ResponseStatusCode >= 400 then
    begin
      try
        ErrJSON := TJSONObject(GetJSON(aRespStr));
      except
        on E: Exception do
          raise EYooKassaError.CreateFmt('Invalid error response from YooKassa: %s', [aRespStr]);
      end;
      raise EYooKassaError.CreateFromResponse(aHTTP.ResponseStatusCode, ErrJSON);
    end;

    Result := TJSONObject(GetJSON(aRespStr));
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

class function TYookassaPayment.ParseJSONResp(const aResp: TJSONObject): String;
var
  aPaymentUrl: TJSONStringType;
begin
  if aResp.FindPath('confirmation.confirmation_url') <> nil then
    aPaymentUrl := aResp.FindPath('confirmation.confirmation_url').AsString
  else if aResp.Find('confirmation') <> nil then
    aPaymentUrl := aResp.Objects['confirmation'].Get('confirmation_url', '')
  else
    aPaymentUrl := '';
  if aPaymentUrl = '' then
    raise Exception.Create('No confirmation_url in Yookassa response: ' + aResp.AsJSON);
  Result := aPaymentUrl;
end;

function TYookassaPayment.CreatePayment: TJSONObject;
begin
  Result := DoCreatePayment;
end;

class function TYookassaPayment.CreatePayment(const aShopId, aSecretKey: string;
  aAmount: Currency; const aCurrency, aDescription, aReturnUrl: string): string;
var
  aPayment: TYookassaPayment;
  aResp: TJSONObject;
begin
  aPayment := TYookassaPayment.Create;
  try
    aPayment.FShopId := aShopId;
    aPayment.FSecretKey := aSecretKey;
    aPayment.FAmount := aAmount;
    aPayment.FCurrency := aCurrency;
    aPayment.FDescription := aDescription;
    aPayment.FReturnUrl := aReturnUrl;
    aResp := aPayment.DoCreatePayment;
    try
      Result:=aPayment.ParseJSONResp(aResp);
    finally
      aResp.Free;
    end;
  finally
    aPayment.Free;
  end;
end;

initialization
  _FrmtStngsJSON:=DefaultFormatSettings;
  _FrmtStngsJSON.DecimalSeparator:='.';
  Randomize;

end.
