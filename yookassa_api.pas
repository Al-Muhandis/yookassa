unit yookassa_api;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fphttpclient, base64, fgl
  ;

type
  TYookassaLogEvent = procedure(aEvent: TEventType; const Msg: string) of object;

  { TYookassaResponse }
  TYookassaResponse = class
  private
    FRaw: TJSONObject;                  
    function GetId: string; virtual; abstract;
    function GetStatus: string; virtual; abstract;
  public
    constructor Create(ARaw: TJSONObject);
    destructor Destroy; override;
    property Raw: TJSONObject read FRaw;
  end;

  { TYookassaPaymentResponse }
  TYookassaPaymentResponse = class(TYookassaResponse) 
  private
    function GetConfirmationURL: string;
    function GetAmount: Currency;   
    function GetId: string; override;
  public
    function GetStatus: string; override;
    property ConfirmationURL: string read GetConfirmationURL;
    property Amount: Currency read GetAmount;
    property ID: String read GetId;
  end;

  { TYookassaReceiptResponse }
  TYookassaReceiptResponse = class(TYookassaResponse)
  private
    function GetPaymentId: String;   
    function GetId: string; override;
  public
    function GetStatus: string; override;
    property PaymentId: string read GetPaymentId;
    property ID: String read GetId;
  end;

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
  protected
    function BuildRequestJSON: string; virtual; abstract;
    function CreateResponse(aRaw: TJSONObject): TYookassaResponse; virtual; abstract;
    function GetEndpoint: string; virtual; abstract;
    function GetMethod: string; virtual;
    function DoExecute: String; virtual;
    procedure Log(aEvent: TEventType; const Msg: string);
  public
    property OnLog: TYookassaLogEvent read FOnLog write FOnLog;
    property ShopId: string read FShopId write FShopId;
    property SecretKey: string read FSecretKey write FSecretKey;
    constructor Create; virtual;
    destructor Destroy; override;
    function Execute: TYookassaResponse;
  end;

  { TYookassaSupplier }
  TYookassaSupplier = class
  private
    FName: string;
    FPhone: string;
    FInn: string;
  public
    constructor Create(const AName, APhone, AInn: string); overload;
    function ToJSON: TJSONObject;
    property Name: string read FName write FName;
    property Phone: string read FPhone write FPhone;
    property Inn: string read FInn write FInn;
  end;

  { TYookassaReceiptItem }
  TYookassaReceiptItem = class
  private
    FSupplier: TYookassaSupplier;
    function GetSupplier: TYookassaSupplier;
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
    destructor Destroy; override;
    function ToJSON: TJSONObject;
    property Supplier: TYookassaSupplier read GetSupplier;
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

  { TYookassaCreateReceiptRequest }
  TYookassaCreateReceiptRequest = class(TYookassaRequest)
  private
    FReceiptType: string;
    FPaymentId: string;
    FRefundId: string;
    FReceipt: TYookassaReceipt;
    FSend: Boolean;
    FSettlements: TJSONArray;
    function GetReceipt: TYookassaReceipt;
  protected
    function BuildRequestJSON: string; override;
    function CreateResponse(aRaw: TJSONObject): TYookassaResponse; override;
    function GetEndpoint: string; override;
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
      const aPaymentId: string = ''; aSend: Boolean = True): String;
  end;

 { TYookassaCreatePaymentRequest }
  TYookassaCreatePaymentRequest = class(TYookassaRequest)
  private
    FAmount: Currency;
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
    function CreateResponse(aRaw: TJSONObject): TYookassaResponse; override;
    function GetEndpoint: string; override;
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
  end;

  { TYookassaGetPaymentRequest }
  TYookassaGetPaymentRequest = class(TYookassaRequest)
  private
    FPaymentId: string;
  protected
    function BuildRequestJSON: string; override;
    function GetEndpoint: string; override;
    function GetMethod: string; override;
    function CreateResponse(aRaw: TJSONObject): TYookassaResponse; override;
  public
    property PaymentId: string read FPaymentId write FPaymentId;
    class function GetPayment(const aShopId, aSecretKey, aPaymentId: string): TYookassaPaymentResponse;
  end;

implementation

uses
  opensslsockets, yookassa_exceptions
  ;

var
  _FrmtStngsJSON: TFormatSettings;

const
  _YK_DEFAULT_API_URL = 'https://api.yookassa.ru/v3';

function IsValidBase64(const AStr: string): Boolean;
var
  aData: String;
begin
  Result := False;
  try
    aData := DecodeStringBase64(AStr);
    Result := Length(aData) > 0;
  except
    on E: Exception do
      Exit(False);
  end;
end;

{ TYookassaResponse }

constructor TYookassaResponse.Create(ARaw: TJSONObject);
begin
  FRaw:=ARaw;
end;

destructor TYookassaResponse.Destroy;
begin
  FRaw.Free;
  inherited Destroy;
end;

{ TYookassaPaymentResponse }

function TYookassaPaymentResponse.GetId: string;
begin
  Result := Raw.Get('id', '');
end;

function TYookassaPaymentResponse.GetStatus: string;
begin
  Result := Raw.Get('status', '');
end;

function TYookassaPaymentResponse.GetConfirmationURL: string;
begin
  if Assigned(Raw.Find('confirmation')) then
    Result := Raw.Objects['confirmation'].Get('confirmation_url', '')
  else
    Result := '';
end;

function TYookassaPaymentResponse.GetAmount: Currency;
var
  ValueStr: string;
begin
  Result := 0;
  ValueStr := Raw.FindPath('amount.value').AsString;
  if not ValueStr.IsEmpty then
    Result := StrToCurr(ValueStr, _FrmtStngsJSON);
end;

{ TYookassaReceiptResponse }

function TYookassaReceiptResponse.GetPaymentId: String;
begin
  Result := Raw.Get('payment_id', EmptyStr);
end;

function TYookassaReceiptResponse.GetId: string;
begin
  Result := Raw.Get('id', EmptyStr);
end;

function TYookassaReceiptResponse.GetStatus: string;
begin
  Result := Raw.Get('status', '');
end;

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
var
  aGUID: TGUID;
begin
  //Result := IntToHex(Random(MaxInt), 8) + IntToStr(Random(MaxInt));
  CreateGUID(aGUID);
  Result:=GUIDToString(aGUID);
  Result := StringReplace(Result, '{', EmptyStr, [rfReplaceAll]);
  Result := StringReplace(Result, '}', EmptyStr, [rfReplaceAll]);
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
        case GetMethod of
          'POST': aRespStr := aHttp.Post(GetEndpoint);
          'GET':  aRespStr := aHttp.Get(GetEndpoint);
        else
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

function TYookassaRequest.Execute: TYookassaResponse;
var
  RespStr: string;
  RespJSON: TJSONObject;
begin
  RespStr := DoExecute;
  RespJSON := TJSONObject(GetJSON(RespStr));
  try
    Result := CreateResponse(RespJSON); // abstract factory method
    RespJSON := nil;
  finally
    RespJSON.Free;
  end;
end;

{ TYookassaSupplier }

constructor TYookassaSupplier.Create(const AName, APhone, AInn: string);
begin
  Create;
  FName := AName;
  FPhone := APhone;
  FInn := AInn;
end;

function TYookassaSupplier.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  if FName <> '' then Result.Add('name', FName);
  if FPhone <> '' then Result.Add('phone', FPhone);
  if FInn <> '' then Result.Add('inn', FInn);
end;

{ TYookassaReceiptItem }

function TYookassaReceiptItem.GetSupplier: TYookassaSupplier;
begin
  if not Assigned(FSupplier) then
    FSupplier := TYookassaSupplier.Create;
  Result := FSupplier;
end;

constructor TYookassaReceiptItem.Create;
begin
  MarkMode := -1; // -1 = not specified
end;

destructor TYookassaReceiptItem.Destroy;
begin
  FSupplier.Free;
  inherited Destroy;
end;

function TYookassaReceiptItem.ToJSON: TJSONObject;
var
  aAmount: TJSONObject;
  aMarkCodeInfo: TJSONObject;
  aMarkCodeBytes: String;
begin
  Result := TJSONObject.Create;
  try
    Result.Add('description', Description);
    Result.Add('quantity', Quantity);
    aAmount := TJSONObject.Create;
    aAmount.Add('value', Format('%.2f', [AmountValue], _FrmtStngsJSON));
    aAmount.Add('currency', AmountCurrency);
    Result.Add('amount', aAmount);
    Result.Add('vat_code', VatCode);
    if PaymentMode <> '' then Result.Add('payment_mode', PaymentMode);
    if PaymentSubject <> '' then Result.Add('payment_subject', PaymentSubject);
    if MarkMode >= 0 then
    begin
      Result.Add('mark_mode', MarkMode);

      // check MarkCodeInfo
      if MarkCodeInfo = '' then
        raise EYooKassaValidationError.Create('MarkCodeInfo is required when MarkMode is set');

      if not IsValidBase64(MarkCodeInfo) then
        raise EYooKassaValidationError.Create('MarkCodeInfo is not a valid base64 string');

      // Optional: length check after decoding
      try
        aMarkCodeBytes := DecodeStringBase64(MarkCodeInfo);
        if (Length(aMarkCodeBytes) < 30) or (Length(aMarkCodeBytes) > 100) then
          raise EYooKassaValidationError.Create('MarkCodeInfo has invalid length after decoding (expected 30–100 bytes)');
      except
        on E: Exception do
          raise EYooKassaValidationError.Create('MarkCodeInfo decoding error: ' + E.Message);
      end;

      // add to JSON
      aMarkCodeInfo := TJSONObject.Create;
      aMarkCodeInfo.Add('gs_1m', MarkCodeInfo);
      Result.Add('mark_code_info', aMarkCodeInfo);
    end;

    if Measure <> '' then
      Result.Add('measure', Measure);

    if Assigned(FSupplier) then
      Result.Add('supplier', FSupplier.ToJSON);

  except
    FreeAndNil(Result);
    raise;
  end;
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

{ TYookassaCreateReceiptRequest }

function TYookassaCreateReceiptRequest.BuildRequestJSON: string;
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

function TYookassaCreateReceiptRequest.CreateResponse(aRaw: TJSONObject): TYookassaResponse;
begin
  Result:=TYookassaReceiptResponse.Create(ARaw);
end;

function TYookassaCreateReceiptRequest.GetEndpoint: string;
begin
  Result := FApiBaseUrl + '/receipts';
end;

constructor TYookassaCreateReceiptRequest.Create;
begin
  FSend := True; // by default, we send the receipt to the client
  FReceiptType := 'payment'; // by default, the payment receipt
end;

destructor TYookassaCreateReceiptRequest.Destroy;
begin
  FSettlements.Free;
  FReceipt.Free;
  inherited Destroy;
end;

function TYookassaCreateReceiptRequest.GetReceipt: TYookassaReceipt;
begin
  if not Assigned(FReceipt) then
    FReceipt:=TYookassaReceipt.Create;
  Result:=FReceipt;
end;

class function TYookassaCreateReceiptRequest.CreateReceipt(const aShopId, aSecretKey: string; aReceipt: TYookassaReceipt;
  const aReceiptType: string; const aPaymentId: string; aSend: Boolean): String;
var
  aReceiptReq: TYookassaCreateReceiptRequest;
  aResp: TYookassaReceiptResponse;
begin
  aReceiptReq := TYookassaCreateReceiptRequest.Create;
  try
    aReceiptReq.FShopId := aShopId;
    aReceiptReq.FSecretKey := aSecretKey;
    aReceiptReq.FReceipt := aReceipt;
    aReceiptReq.FReceiptType := aReceiptType;
    aReceiptReq.FPaymentId := aPaymentId;
    aReceiptReq.FSend := aSend;
    aResp := aReceiptReq.Execute as TYookassaReceiptResponse;
    try
      Result := aResp.GetId;
    finally
      aResp.Free;
    end;
  finally
    aReceiptReq.Free;
  end;
end;

{ TYookassaCreatePaymentRequest }

function TYookassaCreatePaymentRequest.BuildAmountJSON: TJSONObject;
begin
  Result:= TJSONObject.Create;
  Result.Add('value', Format('%.2f', [FAmount], _FrmtStngsJSON));
  Result.Add('currency', FCurrency);
end;

function TYookassaCreatePaymentRequest.BuildConfirmationJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('type', 'redirect');
  Result.Add('return_url', FReturnUrl);
end;

function TYookassaCreatePaymentRequest.BuildMetadataJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('order_id', FMetaOrderId);
end;

function TYookassaCreatePaymentRequest.CreateResponse(aRaw: TJSONObject): TYookassaResponse;
begin
  Result := TYookassaPaymentResponse.Create(ARaw);
end;

function TYookassaCreatePaymentRequest.BuildRequestJSON: string;
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

function TYookassaCreatePaymentRequest.GetEndpoint: string;
begin
  Result := FApiBaseUrl + '/payments';
end;

class function TYookassaCreatePaymentRequest.CreatePayment(const aShopId, aSecretKey: string;
  aAmount: Currency; const aCurrency, aDescription, aReturnUrl: string): string;
var
  aPayment: TYookassaCreatePaymentRequest;
  aResp: TYookassaPaymentResponse;
begin
  aPayment := TYookassaCreatePaymentRequest.Create;
  try
    aPayment.ShopId := aShopId;
    aPayment.SecretKey := aSecretKey;
    aPayment.Amount := aAmount;
    aPayment.Currency := aCurrency;
    aPayment.Description := aDescription;
    aPayment.ReturnUrl := aReturnUrl;
    aResp := aPayment.Execute as TYookassaPaymentResponse;
    try
      Result := aResp.ConfirmationURL;
    finally
      aResp.Free;
    end;
  finally
    aPayment.Free;
  end;
end;

destructor TYookassaCreatePaymentRequest.Destroy;
begin
  FReceipt.Free;
  inherited Destroy;
end;

{ TYookassaGetPaymentRequest }

function TYookassaGetPaymentRequest.GetMethod: string;
begin
  Result := 'GET';
end;

function TYookassaGetPaymentRequest.BuildRequestJSON: string;
begin
  Result := EmptyStr; // The GET request does not have a body
end;

function TYookassaGetPaymentRequest.GetEndpoint: string;
begin
  EYooKassaValidationError.RaiseIfEmpty(FPaymentId, 'PaymentId');
  Result := FApiBaseUrl + '/payments/' + FPaymentId;
end;

function TYookassaGetPaymentRequest.CreateResponse(aRaw: TJSONObject): TYookassaResponse;
begin
  Result := TYookassaPaymentResponse.Create(aRaw);
end;

class function TYookassaGetPaymentRequest.GetPayment(const aShopId, aSecretKey,
  aPaymentId: string): TYookassaPaymentResponse;
var
  aReq: TYookassaGetPaymentRequest;
begin
  Result := nil;
  aReq := self.Create;
  try
    aReq.ShopId := aShopId;
    aReq.SecretKey := aSecretKey;
    aReq.PaymentId := aPaymentId;

    // Make request
    Result := aReq.Execute as TYookassaPaymentResponse;
  finally
    aReq.Free;
  end;
end;

initialization
  _FrmtStngsJSON:=DefaultFormatSettings;
  _FrmtStngsJSON.DecimalSeparator:='.';
  Randomize;

end.
