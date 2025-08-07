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
    function GetStatus: string; override;
  public
    property ConfirmationURL: string read GetConfirmationURL;
    property Amount: Currency read GetAmount;
    property ID: String read GetId;
    property Status: String read GetStatus;
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
    function GetMethod: string; virtual; abstract;
    function DoExecute: String; virtual;
    procedure Log(aEvent: TEventType; const Msg: string);
  public
    property OnLog: TYookassaLogEvent read FOnLog write FOnLog;
    property ShopId: string read FShopId write FShopId;
    property SecretKey: string read FSecretKey write FSecretKey;
    constructor Create; virtual;
    function Execute: TYookassaResponse;
  end;

  TYookassaAPIObject = class
  public
    function ToJSON: TJSONObject; virtual; abstract;
  end;

  { TYookassaSupplier }
  TYookassaSupplier = class(TYookassaAPIObject)
  private
    FName: string;
    FPhone: string;
    FInn: string;
  public
    constructor Create(const AName, APhone, AInn: string); overload;
    function ToJSON: TJSONObject; override;
    property Name: string read FName write FName;
{ Телефон поставщика (тег в 54 ФЗ — 1171). Указывается в формате ITU-T E.164, например 79000000000.
  Параметр предусмотрен форматом фискальных документов (ФФД) и является обязательным, начиная с версии 1.1. }
    property Phone: string read FPhone write FPhone;
    property Inn: string read FInn write FInn;
  end;

  { Types of agents according to ФФД 1.1 }
  TYookassaAgentType = (
    atNone,                 // Not specified
    atBankingPaymentAgent,  // banking_payment_agent
    atBankingPaymentSubagent, // banking_payment_subagent
    atPaymentAgent,         // payment_agent
    atPaymentSubagent,      // payment_subagent
    atAttorney,             // attorney
    atCommissioner,         // commissioner
    atAgent                 // agent
  );

  { TYookassaReceiptItem }
  TYookassaReceiptItem = class(TYookassaAPIObject)
  private
    FAgentType: TYookassaAgentType;
    FSupplier: TYookassaSupplier;
    function GetAgentTypeString: string;
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
    function ToJSON: TJSONObject; override;
    property AgentType: TYookassaAgentType read FAgentType write FAgentType;
    property Supplier: TYookassaSupplier read GetSupplier;
  end;

  TReceiptItems = specialize TFPGObjectList<TYookassaReceiptItem>;

  { TYookassaReceipt }
  TYookassaReceipt = class(TYookassaAPIObject)
  private
    FCustomerEmail: string;
    FCustomerPhone: string;
    FItems: TReceiptItems;
    FTaxSystemCode: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddItem(aItem: TYookassaReceiptItem);
    function ToJSON: TJSONObject;  override;
    procedure AppendJSON(aJSON: TJSONObject);
    property CustomerEmail: String read FCustomerEmail write FCustomerEmail;
    property CustomerPhone: String read FCustomerPhone write FCustomerPhone;
    property Items: TReceiptItems read FItems write FItems;
    property TaxSystemCode: Integer read FTaxSystemCode write FTaxSystemCode;
  end;

  { TYookassaSettlement }
  TYookassaSettlement = class(TYookassaAPIObject)
  private
    FType: string;     // 'cash' or 'bank_card'
    FAmountValue: Currency;
    FAmountCurrency: string;
  public
    constructor Create; overload;
    constructor Create(const aType: string; aAmount: Currency; const aCurrency: string); overload;
    function ToJSON: TJSONObject; override;
    property SettlementType: string read FType write FType;
    property Amount: Currency read FAmountValue write FAmountValue;
    property Currency: string read FAmountCurrency write FAmountCurrency;
  end;

  TSettlementsList = specialize TFPGObjectList<TYookassaSettlement>;

  { TYookassaCreateReceiptRequest }
  TYookassaCreateReceiptRequest = class(TYookassaRequest)
  private
    FReceiptType: string;
    FPaymentId: string;
    FRefundId: string;
    FReceipt: TYookassaReceipt;
    FSend: Boolean;
    FSettlements: TSettlementsList;
    function GetReceipt: TYookassaReceipt;
    function GetSettlements: TSettlementsList;
  protected
    function BuildRequestJSON: string; override;
    function CreateResponse(aRaw: TJSONObject): TYookassaResponse; override;
    function GetEndpoint: string; override;
    function GetMethod: string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property ReceiptType: string read FReceiptType write FReceiptType;
    property PaymentId: string read FPaymentId write FPaymentId;
    property RefundId: string read FRefundId write FRefundId;
    property Receipt: TYookassaReceipt read GetReceipt write FReceipt;
    property Send: Boolean read FSend write FSend;
    property Settlements: TSettlementsList read GetSettlements;
    class function CreateReceipt(const aShopId, aSecretKey: string; aReceipt: TYookassaReceipt;
      const aReceiptType: string; const aPaymentId: string; aSend: Boolean): TYookassaReceiptResponse;
  end;

  { TYookassaReceiverType }
  TYookassaReceiverType = (rtUnknown, rtBankAccount, rtMobileBalance, rtDigitalWallet);

  { TYookassaReceiver }
  TYookassaReceiver = class
  private
    FReceiverType: TYookassaReceiverType;
    FAccountNumber: string;
    FBic: string;
    FPhone: string;
    function GetTypeString: string;
  public
    constructor Create; overload;
    constructor Create(aType: TYookassaReceiverType); overload;
    function ToJSON: TJSONObject;
    property ReceiverType: TYookassaReceiverType read FReceiverType write FReceiverType;
    property AccountNumber: string read FAccountNumber write FAccountNumber;
    property Bic: string read FBic write FBic;
    property Phone: string read FPhone write FPhone;
  end;

 { TYookassaCreatePaymentRequest }
  TYookassaCreatePaymentRequest = class(TYookassaRequest)
  private
    FAmount: Currency;
    FCurrency: string;
    FDescription: string;        
    FMetaOrderId: string;
    FReturnUrl: string;
    FReceipt: TYookassaReceipt;
    FReceiver: TYookassaReceiver;
    function BuildAmountJSON: TJSONObject;
    function BuildConfirmationJSON: TJSONObject;
    function BuildMetadataJSON: TJSONObject;
    function GetReceiver: TYookassaReceiver;
  protected
    function BuildRequestJSON: string; override;  
    function CreateResponse(aRaw: TJSONObject): TYookassaResponse; override;
    function GetEndpoint: string; override;
    function GetMethod: string; override;
  public
    destructor Destroy; override;
    property Amount: Currency read FAmount write FAmount;
    property Currency: string read FCurrency write FCurrency;
    property Description: string read FDescription write FDescription;
    property Receiver: TYookassaReceiver read GetReceiver;
    property ReturnUrl: string read FReturnUrl write FReturnUrl;
    property Receipt: TYookassaReceipt read FReceipt write FReceipt;
    property MetaOrderId: string read FMetaOrderId write FMetaOrderId;
    class function CreatePayment(const aShopId, aSecretKey: string; aAmount: Currency; const aCurrency, aDescription,
      aReturnUrl: string): TYookassaPaymentResponse;
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

      Log(etDebug, Format('Request (Idempotence-Key - %s) (%s %s):%s%s',
        [aIdempotenceKey, GetMethod, GetEndpoint, LineEnding, aReqJSON]));

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
  try
    if FName <> '' then Result.Add('name', FName);
    if FPhone <> '' then Result.Add('phone', FPhone);
    if FInn <> '' then Result.Add('inn', FInn);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TYookassaReceiptItem }

function TYookassaReceiptItem.GetAgentTypeString: string;
begin
  case FAgentType of
    atBankingPaymentAgent:   Result := 'banking_payment_agent';
    atBankingPaymentSubagent: Result := 'banking_payment_subagent';
    atPaymentAgent:          Result := 'payment_agent';
    atPaymentSubagent:       Result := 'payment_subagent';
    atAttorney:              Result := 'attorney';
    atCommissioner:          Result := 'commissioner';
    atAgent:                 Result := 'agent';
  else
    Result := EmptyStr;
  end;
end;

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
  aMarkCodeBytes, aAgentTypeStr: String;
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

    // agent_type (ФФД 1.1)
    aAgentTypeStr := GetAgentTypeString;
    if aAgentTypeStr <> EmptyStr then
      Result.Add('agent_type', aAgentTypeStr);

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
begin
  Result := TJSONObject.Create;
  try
    AppendJSON(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TYookassaReceipt.AppendJSON(aJSON: TJSONObject);
var
  aItems: TJSONArray;
  aCustomer: TJSONObject;
  Item: TYookassaReceiptItem;
begin
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
end;

{ TYookassaSettlement }

constructor TYookassaSettlement.Create;
begin
  FAmountCurrency := 'RUB';
end;

constructor TYookassaSettlement.Create(const aType: string; aAmount: Currency; const aCurrency: string);
begin
  Create;
  FType := aType;
  FAmountValue := aAmount;
  FAmountCurrency := aCurrency;
end;

function TYookassaSettlement.ToJSON: TJSONObject;
var
  aAmount: TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    if FType <> '' then
      Result.Add('type', FType);

    aAmount := TJSONObject.Create;
    aAmount.Add('value', Format('%.2f', [FAmountValue], _FrmtStngsJSON));
    aAmount.Add('currency', FAmountCurrency);
    Result.Add('amount', aAmount);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TYookassaCreateReceiptRequest }

function TYookassaCreateReceiptRequest.BuildRequestJSON: string;
var
  aJsonReq: TJSONObject;
  aSettlementsArray: TJSONArray;
  aSettlement: TYookassaSettlement;
begin
  EYooKassaValidationError.RaiseIfNil(FReceipt, 'Receipt');
  EYooKassaValidationError.RaiseIfFalse(FReceipt.Items.Count > 0, 'Receipt must have at least one item');

  if FReceiptType = 'refund' then
    EYooKassaValidationError.RaiseIfFalse((FPaymentId <> '') or (FRefundId <> ''),
      'For refund receipt, either PaymentId or RefundId must be specified');

  aJsonReq := TJSONObject.Create;
  try
    // receipt type (payment/refund)
    aJsonReq.Add('type', FReceiptType);

    // whether to send the receipt
    aJsonReq.Add('send', FSend);

    // receipt data
    if Assigned(FReceipt) then
      FReceipt.AppendJSON(aJsonReq);

    // The refund receipt requires a payment_id or a refund_id.
    if FReceiptType = 'refund' then begin
      if FPaymentId <> '' then
        aJsonReq.Add('payment_id', FPaymentId)
      else if FRefundId <> '' then
        aJsonReq.Add('refund_id', FRefundId);
    end
    else
      aJsonReq.Add('payment_id', FPaymentId);

    // A list of settlings or payments
    if Assigned(FSettlements) and (FSettlements.Count > 0) then
    begin
      aSettlementsArray := TJSONArray.Create;
      for aSettlement in FSettlements do
        aSettlementsArray.Add(aSettlement.ToJSON);
      aJsonReq.Add('settlements', aSettlementsArray);
    end;

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

function TYookassaCreateReceiptRequest.GetMethod: string;
begin
  Result:='POST';
end;

constructor TYookassaCreateReceiptRequest.Create;
begin
  inherited Create;
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

function TYookassaCreateReceiptRequest.GetSettlements: TSettlementsList;
begin
  if not Assigned(FSettlements) then
    FSettlements := TSettlementsList.Create(True); // Auto-free
  Result := FSettlements;
end;

class function TYookassaCreateReceiptRequest.CreateReceipt(const aShopId, aSecretKey: string;
  aReceipt: TYookassaReceipt; const aReceiptType: string; const aPaymentId: string;
  aSend: Boolean): TYookassaReceiptResponse;
var
  aReceiptReq: TYookassaCreateReceiptRequest;
begin
  aReceiptReq := self.Create;
  try
    aReceiptReq.ShopId := aShopId;
    aReceiptReq.SecretKey := aSecretKey;
    aReceiptReq.Receipt := aReceipt;
    aReceiptReq.ReceiptType := aReceiptType;
    aReceiptReq.PaymentId := aPaymentId;
    aReceiptReq.Send := aSend;

    Result := aReceiptReq.Execute as TYookassaReceiptResponse;
  finally
    aReceiptReq.Free;
  end;
end;

{ TYookassaReceiver }

constructor TYookassaReceiver.Create;
begin
  inherited Create;
  FReceiverType := rtBankAccount;
end;

constructor TYookassaReceiver.Create(aType: TYookassaReceiverType);
begin
  inherited Create;
  FReceiverType := aType;
end;

function TYookassaReceiver.GetTypeString: string;
begin
  case FReceiverType of
    rtBankAccount: Result := 'bank_account';
    rtMobileBalance: Result := 'mobile_balance';
    rtDigitalWallet: Result := 'digital_wallet';
  else
    Result := EmptyStr;
  end;
end;

function TYookassaReceiver.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.Add('type', GetTypeString);

    case FReceiverType of
      rtBankAccount:
        begin
          EYooKassaValidationError.RaiseIfEmpty(FAccountNumber, 'AccountNumber');
          EYooKassaValidationError.RaiseIfEmpty(FBic, 'Bic');
          Result.Add('account_number', FAccountNumber);
          Result.Add('bic', FBic);
        end;
      rtMobileBalance:
        begin
          EYooKassaValidationError.RaiseIfEmpty(FPhone, 'Phone');
          Result.Add('phone', FPhone);
        end;
      rtDigitalWallet:
        begin
          EYooKassaValidationError.RaiseIfEmpty(FAccountNumber, 'AccountNumber');
          Result.Add('account_number', FAccountNumber);
        end;
    end;
  except
    FreeAndNil(Result);
    raise;
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

function TYookassaCreatePaymentRequest.GetReceiver: TYookassaReceiver;
begin
  if not Assigned(FReceiver) then
    FReceiver := TYookassaReceiver.Create(rtBankAccount);
  Result := FReceiver;
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
    // receiver (for payout to bank, phone, wallet)
    if Assigned(FReceiver) then
      aJsonReq.Add('receiver', FReceiver.ToJSON);  
    Result := aJsonReq.AsJSON;
  finally
    aJsonReq.Free;
  end;
end;

function TYookassaCreatePaymentRequest.GetEndpoint: string;
begin
  Result := FApiBaseUrl + '/payments';
end;

function TYookassaCreatePaymentRequest.GetMethod: string;
begin
  Result:='POST';
end;

class function TYookassaCreatePaymentRequest.CreatePayment(const aShopId, aSecretKey: string; aAmount: Currency;
  const aCurrency, aDescription, aReturnUrl: string): TYookassaPaymentResponse;
var
  aPayment: TYookassaCreatePaymentRequest;
begin
  aPayment := self.Create;
  try
    aPayment.ShopId := aShopId;
    aPayment.SecretKey := aSecretKey;
    aPayment.Amount := aAmount;
    aPayment.Currency := aCurrency;
    aPayment.Description := aDescription;
    aPayment.ReturnUrl := aReturnUrl;

    Result := aPayment.Execute as TYookassaPaymentResponse;
  finally
    aPayment.Free;
  end;
end;

destructor TYookassaCreatePaymentRequest.Destroy;
begin
  FReceiver.Free;
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
