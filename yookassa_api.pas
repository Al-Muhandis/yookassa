unit yookassa_api;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fphttpclient, base64, fgl
  ;

type
  TYookassaLogEvent = procedure(aEvent: TEventType; const Msg: string) of object;

  { Base class for request at YooKassa API }
  { TYookassaRequest }

  TYookassaRequest = class
  private
    FApiBaseUrl: string;
    FOnLog: TYookassaLogEvent;
    FShopId: string;
    FSecretKey: string;                     
    function GenerateIdempotenceKey: string;
    function GetAuthHeader: string;
    function GetDefaultApiBaseUrl: string;
    procedure Log(aEvent: TEventType; const Msg: string);
  protected
    function BuildRequestJSON: string; virtual; abstract;
    function GetEndpoint: string; virtual; abstract;
    function GetMethod: string; virtual;
    function ParseResponse(const AResponse: String): TJSONObject; virtual;
    function DoExecute: String;
  public
    property OnLog: TYookassaLogEvent read FOnLog write FOnLog;
    property ShopId: string read FShopId write FShopId;
    property SecretKey: string read FSecretKey write FSecretKey;
    constructor Create; virtual;
    destructor Destroy; override;
    function Execute: TJSONObject;
  end;

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

  TReceiptItems = specialize TFPGObjectList<TYookassaReceiptItem>;

  { TYookassaReceipt }
  TYookassaReceipt = class
  private
    FCustomerEmail: string;
    FCustomerPhone: string;
    FItems: TReceiptItems;
    FTaxSystemCode: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddItem(aItem: TYookassaReceiptItem);
    function ToJSON: TJSONObject;
    function ToReceiptJSON: TJSONObject; // to create a receipt separately
    property CustomerEmail: String read FCustomerEmail write FCustomerEmail;
    property CustomerPhone: String read FCustomerPhone write FCustomerPhone;
    property Items: TReceiptItems read FItems write FItems;
    property TaxSystemCode: Integer read FTaxSystemCode write FTaxSystemCode;
  end;

  { TYookassaReceiptRequest }
  TYookassaReceiptRequest = class(TYookassaRequest)
  private
    FReceiptID: String;
    FReceiptType: string;
    FPaymentId: string;
    FRefundId: string;
    FReceipt: TYookassaReceipt;
    FSend: Boolean;
    FSettlements: TJSONArray;
    FStatus: String;
    function GetReceipt: TYookassaReceipt;
  protected
    function BuildRequestJSON: string; override;
    function GetEndpoint: string; override;
    function ParseResponse(const AResponse: String): TJSONObject; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property ReceiptType: string read FReceiptType write FReceiptType;
    property PaymentId: string read FPaymentId write FPaymentId;
    property RefundId: string read FRefundId write FRefundId;
    property Receipt: TYookassaReceipt read GetReceipt write FReceipt;
    property Send: Boolean read FSend write FSend;
    property Settlements: TJSONArray read FSettlements write FSettlements;
    class function CreateReceipt(const aShopId, aSecretKey: string;
      aReceipt: TYookassaReceipt; const aReceiptType: string = 'payment';
      const aPaymentId: string = ''; aSend: Boolean = True): TJSONObject;
    property ReceiptID: String read FReceiptID;
    property Status: String read FStatus;
  end;

 { TYookassaPaymentRequest }
  TYookassaPaymentRequest = class(TYookassaRequest)
  private
    FAmount: Currency;
    FConfirmationURL: String;
    FCurrency: string;
    FDescription: string;
    FReturnUrl: string;
    FReceipt: TYookassaReceipt;
    FMetaOrderId: string;
    function BuildAmountJSON: TJSONObject;
    function BuildConfirmationJSON: TJSONObject;
    function BuildMetadataJSON: TJSONObject;
  protected
    function BuildRequestJSON: string; override;
    function GetEndpoint: string; override;
    function ParseResponse(const AResponse: String): TJSONObject; override;
  public
    destructor Destroy; override;
    property Amount: Currency read FAmount write FAmount;
    property Currency: string read FCurrency write FCurrency;
    property Description: string read FDescription write FDescription;
    property ReturnUrl: string read FReturnUrl write FReturnUrl;
    property Receipt: TYookassaReceipt read FReceipt write FReceipt;
    property MetaOrderId: string read FMetaOrderId write FMetaOrderId;
    class function CreatePayment(const aShopId, aSecretKey: string; aAmount: Currency;
      const aCurrency, aDescription, aReturnUrl: string): string;
    property ConfirmationURL: String read FConfirmationURL;
  end;

implementation

uses
  opensslsockets, yookassa_exceptions
  ;

var
  _FrmtStngsJSON: TFormatSettings;

const
  _YK_DEFAULT_API_URL = 'https://api.yookassa.ru/v3';

{ TYookassaRequest }

constructor TYookassaRequest.Create;
begin
  inherited Create;
  FApiBaseUrl := GetDefaultApiBaseUrl;
end;

destructor TYookassaRequest.Destroy;
begin
  inherited Destroy;
end;

function TYookassaRequest.GetAuthHeader: string;
begin
  Result := 'Basic ' + EncodeStringBase64(FShopId + ':' + FSecretKey);
end;

function TYookassaRequest.GetDefaultApiBaseUrl: string;
begin
  Result := _YK_DEFAULT_API_URL;
end;

function TYookassaRequest.GenerateIdempotenceKey: string;
begin
  Result := IntToHex(Random(MaxInt), 8) + IntToStr(Random(MaxInt));
end;

procedure TYookassaRequest.Log(aEvent: TEventType; const Msg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(aEvent, Msg);
end;

function TYookassaRequest.GetMethod: string;
begin
  Result := 'POST';
end;

function TYookassaRequest.ParseResponse(const AResponse: String): TJSONObject;
begin
  Result:=TJSONObject(GetJSON(AResponse));
end;

function TYookassaRequest.DoExecute: String;
var
  aHttp: TFPHttpClient;
  aRespStr: RawByteString;
  aReqStream: TStringStream;
  ErrJSON: TJSONObject;
  aReqJSON, aIdempotenceKey: String;
begin
  Result := EmptyStr;
  try
    aHttp := TFPHttpClient.Create(nil);
    try
      // --- PRE-REQUEST VALIDATION ---
      EYooKassaValidationError.RaiseIfEmpty(FShopId, 'ShopId');
      EYooKassaValidationError.RaiseIfEmpty(FSecretKey, 'SecretKey');

      aReqJSON:=BuildRequestJSON;

      aHttp.AddHeader('Authorization', GetAuthHeader);
      aHttp.AddHeader('Content-Type', 'application/json');
      aIdempotenceKey:=GenerateIdempotenceKey;
      aHttp.AddHeader('Idempotence-Key', aIdempotenceKey);

      Log(etDebug, Format('YooKassa Request (%s): %s. Idempotence-Key: %s', [GetEndpoint, aReqJSON, aIdempotenceKey]));

      aReqStream := TStringStream.Create(aReqJSON);
      try
        aHttp.RequestBody := aReqStream;
        if GetMethod = 'POST' then
          aRespStr := aHttp.Post(GetEndpoint)
        else
        begin
          raise Exception.Create('HTTP method not supported: ' + GetMethod);
        end;
      finally
        aHttp.RequestBody.Free;
      end;
      Log(etDebug, Format('Response (status: %d): %s', [aHttp.ResponseStatusCode, aRespStr]));

      // Checking the status (API error)
      if aHttp.ResponseStatusCode >= 400 then
      begin
        try
          ErrJSON := TJSONObject(GetJSON(aRespStr));
        except
          on E: Exception do
            raise EYooKassaError.CreateFmt('Invalid error response from YooKassa: %s', [aRespStr]);
        end;
        raise EYooKassaError.CreateFromResponse(aHttp.ResponseStatusCode, ErrJSON);
      end;

      Result := aRespStr;
    finally
      aHttp.Free;
    end;
  except
    on E: Exception do
    begin
      Log(etError, 'Error: ' + E.Message);
      raise; // passing on
    end;
  end;
end;

function TYookassaRequest.Execute: TJSONObject;
var
  aResp: String;
begin
  aResp := DoExecute;
  Result := ParseResponse(aResp); // can be modified in descendants
end;

{ TYookassaReceiptItem }

constructor TYookassaReceiptItem.Create;
begin
  MarkMode := -1; // -1 = not specified
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
  Items := TReceiptItems.Create(True);
  FTaxSystemCode := -1; // not specified
end;

destructor TYookassaReceipt.Destroy;
begin
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
  Item: TYookassaReceiptItem;
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
  for Item in FItems do
    aItems.Add(Item.ToJSON);
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

function TYookassaReceiptRequest.BuildRequestJSON: string;
var
  aJsonReq: TJSONObject;
begin
  EYooKassaValidationError.RaiseIfNil(FReceipt, 'Receipt');
  EYooKassaValidationError.RaiseIfFalse(FReceipt.Items.Count > 0, 'Receipt must have at least one item');

  if FReceiptType = 'refund' then
  begin
    EYooKassaValidationError.RaiseIfFalse(
      (FPaymentId <> '') or (FRefundId <> ''),
      'For refund receipt, either PaymentId or RefundId must be specified'
    );
  end;

  aJsonReq := TJSONObject.Create;
  try
    // receipt type (payment/refund)
    aJsonReq.Add('type', FReceiptType);

    // whether to send the receipt
    aJsonReq.Add('send', FSend);

    // receipt data
    if Assigned(FReceipt) then
      aJsonReq.Add('receipt', FReceipt.ToReceiptJSON);

    // The refund receipt requires a payment_id or a refund_id.
    if FReceiptType = 'refund' then begin
      if FPaymentId <> '' then
        aJsonReq.Add('payment_id', FPaymentId)
      else if FRefundId <> '' then
        aJsonReq.Add('refund_id', FRefundId);
    end;

    // settlements for splitting payments (if used)
    if Assigned(FSettlements) and (FSettlements.Count > 0) then
      aJsonReq.Add('settlements', FSettlements);

    Result := aJsonReq.AsJSON;
  finally
    aJsonReq.Free;
  end;
end;

function TYookassaReceiptRequest.GetEndpoint: string;
begin
  Result := FApiBaseUrl + '/receipts';
end;

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

function TYookassaReceiptRequest.GetReceipt: TYookassaReceipt;
begin
  if not Assigned(FReceipt) then
    FReceipt:=TYookassaReceipt.Create;
  Result:=FReceipt;
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
    Result := aReceiptReq.Execute;
  finally
    aReceiptReq.Free;
  end;
end;

function TYookassaReceiptRequest.ParseResponse(const AResponse: String): TJSONObject;
begin
  Result:=inherited ParseResponse(AResponse);
    // Get receipt ID
  FReceiptID := Result.Get('id', EmptyStr);
  FStatus := Result.Get('status', EmptyStr);
end;

{ TYookassaPaymentRequest }

function TYookassaPaymentRequest.BuildAmountJSON: TJSONObject;
begin
  Result:= TJSONObject.Create;
  Result.Add('value', Format('%.2f', [FAmount], _FrmtStngsJSON));
  Result.Add('currency', FCurrency);
end;

function TYookassaPaymentRequest.BuildConfirmationJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('type', 'redirect');
  Result.Add('return_url', FReturnUrl);
end;

function TYookassaPaymentRequest.BuildMetadataJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('order_id', FMetaOrderId);
end;

function TYookassaPaymentRequest.BuildRequestJSON: string;
var
  aJsonReq: TJSONObject;
begin
  EYooKassaValidationError.RaiseIfZeroOrNegative(FAmount, 'Amount');
  EYooKassaValidationError.RaiseIfEmpty(FCurrency, 'Currency');
  EYooKassaValidationError.RaiseIfEmpty(FDescription, 'Description');
  EYooKassaValidationError.RaiseIfEmpty(FReturnUrl, 'ReturnUrl');

  aJsonReq := TJSONObject.Create;
  try
    aJsonReq.Add('amount', BuildAmountJSON);
    aJsonReq.Add('description', FDescription);
    aJsonReq.Add('confirmation', BuildConfirmationJSON);
    aJsonReq.Add('capture', True);
    if FMetaOrderId <> '' then
      aJsonReq.Add('metadata', BuildMetadataJSON);
    if Assigned(FReceipt) then
      aJsonReq.Add('receipt', FReceipt.ToJSON);
    Result := aJsonReq.AsJSON;
  finally
    aJsonReq.Free;
  end;
end;

function TYookassaPaymentRequest.GetEndpoint: string;
begin
  Result := FApiBaseUrl + '/payments';
end;

function TYookassaPaymentRequest.ParseResponse(const AResponse: String): TJSONObject;
begin
  Result:=inherited ParseResponse(AResponse);
  if Assigned(Result.Find('confirmation')) then
    FConfirmationURL := Result.Objects['confirmation'].Get('confirmation_url', '')
  else
    FCOnfirmationURL := EmptyStr;
end;

class function TYookassaPaymentRequest.CreatePayment(const aShopId, aSecretKey: string;
  aAmount: Currency; const aCurrency, aDescription, aReturnUrl: string): string;
var
  aPayment: TYookassaPaymentRequest;
  aResp: TJSONObject;
begin
  aPayment := TYookassaPaymentRequest.Create;
  try
    aPayment.ShopId := aShopId;
    aPayment.SecretKey := aSecretKey;
    aPayment.Amount := aAmount;
    aPayment.Currency := aCurrency;
    aPayment.Description := aDescription;
    aPayment.ReturnUrl := aReturnUrl;
    aResp := aPayment.Execute;
    try
      Result := aResp.Get('confirmation_url', '');
    finally
      aResp.Free;
    end;
  finally
    aPayment.Free;
  end;
end;

destructor TYookassaPaymentRequest.Destroy;
begin
  FReceipt.Free;
  inherited Destroy;
end;

initialization
  _FrmtStngsJSON:=DefaultFormatSettings;
  _FrmtStngsJSON.DecimalSeparator:='.';
  Randomize;

end.
