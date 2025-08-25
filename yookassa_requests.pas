unit yookassa_requests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fphttpclient, yookassa_models, yookassa_exceptions, yookassa_responses
  ;

type
  // Event для логирования
  TYookassaLogEvent = procedure(aEvent: TEventType; const Msg: string) of object;

  { TYookassaRequest }
  TYookassaRequest = class(TYookassaAPIObject)
  private
   FApiBaseUrl: string;
   FOnLog: TYookassaLogEvent;
   FShopId: string;
   FSecretKey: string;
   function BuildRequestJSON: string;
   function GenerateIdempotenceKey: string;
   function GetAuthHeader: string;
   function GetDefaultApiBaseUrl: string;
  protected
{ Raw должен уничтожаться при освобождении экземпляра ответа TYookassaResponse.
  Для этого в наследниках конструктор TYookassaResponse должен вызываться с параметром (OwnsRaw = True) }
   function CreateResponse(aRaw: TJSONObject): TYookassaResponse; virtual; abstract;
   function GetEndpoint: string; virtual; abstract;
   function GetMethod: string; virtual; abstract;
   function DoExecute: String; virtual;
   procedure Log(aEvent: TEventType; const Msg: string);
  public
   property OnLog: TYookassaLogEvent read FOnLog write FOnLog;
   property ShopId: string read FShopId write FShopId;
   property SecretKey: string read FSecretKey write FSecretKey;
   constructor Create; overload; virtual;
   function Execute: TYookassaResponse;
  end;

  { Создание платежа }
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
    function CreateResponse(aRaw: TJSONObject): TYookassaResponse; override;
    function GetEndpoint: string; override;
    function GetMethod: string; override;
  public
    destructor Destroy; override;
    function ToJSON: TJSONObject; override;
    property Amount: Currency read FAmount write FAmount;
    property &Currency: string read FCurrency write FCurrency;
    property Description: string read FDescription write FDescription;
    property Receiver: TYookassaReceiver read GetReceiver;
    property ReturnUrl: string read FReturnUrl write FReturnUrl;
    property Receipt: TYookassaReceipt read FReceipt write FReceipt;
    property MetaOrderId: string read FMetaOrderId write FMetaOrderId;
    class function CreatePayment(const aShopId, aSecretKey: string; aAmount: Currency; const aCurrency, aDescription,
      aReturnUrl: string): TYookassaPaymentResponse;
  end;

  { Получение информации о платеже }
  { TYookassaGetPaymentRequest }
  TYookassaGetPaymentRequest = class(TYookassaRequest)
  private
    FPaymentId: string;
  protected
    function GetEndpoint: string; override;
    function GetMethod: string; override;
    function CreateResponse(aRaw: TJSONObject): TYookassaResponse; override;
  public
    function ToJSON: TJSONObject; override;
    property PaymentId: string read FPaymentId write FPaymentId;
    class function GetPayment(const aShopId, aSecretKey, aPaymentId: string): TYookassaPaymentResponse;
  end;

  { Создание чека }
  { TYookassaCreateReceiptRequest }
  TYookassaCreateReceiptRequest = class(TYookassaRequest)
  private
    FPaymentId: string;
    FReceiptType: TReceiptType;
    FRefundId: string;
    FReceipt: TYookassaReceipt;
    FSend: Boolean;
    FSettlements: TSettlementsList;
    function GetReceipt: TYookassaReceipt;
    function GetSettlements: TSettlementsList;
  protected
    function CreateResponse(aRaw: TJSONObject): TYookassaResponse; override;
    function GetEndpoint: string; override;
    function GetMethod: string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ToJSON: TJSONObject; override;
    property ReceiptType: TReceiptType read FReceiptType write FReceiptType;
    property PaymentId: string read FPaymentId write FPaymentId;
    property RefundId: string read FRefundId write FRefundId;
    property Receipt: TYookassaReceipt read GetReceipt write FReceipt;
    property Send: Boolean read FSend write FSend;
    property Settlements: TSettlementsList read GetSettlements;
    class function CreateReceipt(const aShopId, aSecretKey: string; aReceipt: TYookassaReceipt; aReceiptType: TReceiptType;
      const aPaymentId: string; aSend: Boolean): TYookassaReceiptResponse;
  end;

implementation

uses
  base64, opensslsockets
  ;

const
  _YK_DEFAULT_API_URL = 'https://api.yookassa.ru/v3';

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
  CreateGUID(aGUID);
  Result := GUIDToString(aGUID);
  Result := StringReplace(Result, '{', EmptyStr, [rfReplaceAll]);
  Result := StringReplace(Result, '}', EmptyStr, [rfReplaceAll]);
end;

procedure TYookassaRequest.Log(aEvent: TEventType; const Msg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(aEvent, Msg);
end;

function TYookassaRequest.BuildRequestJSON: string;
var
  aJSON: TJSONObject;
begin
  aJSON := ToJSON;
  try
    if Assigned(aJSON) then
      Result := aJSON.AsJSON
    else
      Result := EmptyStr;
  finally
    aJSON.Free;
  end;
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

      aReqJSON := BuildRequestJSON;

      aHttp.AddHeader('Authorization', GetAuthHeader);
      aHttp.AddHeader('Content-Type', 'application/json');
      aIdempotenceKey := GenerateIdempotenceKey;
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

{ TYookassaCreatePaymentRequest }

function TYookassaCreatePaymentRequest.BuildAmountJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
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
  Result := TYookassaPaymentResponse.Create(aRaw, True);
end;

function TYookassaCreatePaymentRequest.GetEndpoint: string;
begin
  Result := FApiBaseUrl + '/payments';
end;

function TYookassaCreatePaymentRequest.GetMethod: string;
begin
  Result := 'POST';
end;

class function TYookassaCreatePaymentRequest.CreatePayment(const aShopId, aSecretKey: string;
  aAmount: Currency; const aCurrency, aDescription, aReturnUrl: string): TYookassaPaymentResponse;
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

function TYookassaCreatePaymentRequest.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    EYooKassaValidationError.RaiseIfZeroOrNegative(FAmount, 'Amount');
    EYooKassaValidationError.RaiseIfEmpty(FCurrency, 'Currency');
    EYooKassaValidationError.RaiseIfEmpty(FDescription, 'Description');
    EYooKassaValidationError.RaiseIfEmpty(FReturnUrl, 'ReturnUrl');
    Result.Add('amount', BuildAmountJSON);
    Result.Add('description', FDescription);
    Result.Add('confirmation', BuildConfirmationJSON);
    Result.Add('capture', True);
    if FMetaOrderId <> EmptyStr then
      Result.Add('metadata', BuildMetadataJSON);
    if Assigned(FReceipt) then
      Result.Add('receipt', FReceipt.ToJSON);
    // receiver (for payout to bank, phone, wallet)
    if Assigned(FReceiver) then
      Result.Add('receiver', FReceiver.ToJSON);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TYookassaGetPaymentRequest }

function TYookassaGetPaymentRequest.GetMethod: string;
begin
  Result := 'GET';
end;

function TYookassaGetPaymentRequest.GetEndpoint: string;
begin
  EYooKassaValidationError.RaiseIfEmpty(FPaymentId, 'PaymentId');
  Result := FApiBaseUrl + '/payments/' + FPaymentId;
end;

function TYookassaGetPaymentRequest.CreateResponse(aRaw: TJSONObject): TYookassaResponse;
begin
  Result := TYookassaPaymentResponse.Create(aRaw, True);
end;

function TYookassaGetPaymentRequest.ToJSON: TJSONObject;
begin
  Result := nil; // The GET request does not have a body
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

{ TYookassaCreateReceiptRequest }

function TYookassaCreateReceiptRequest.CreateResponse(aRaw: TJSONObject): TYookassaResponse;
begin
  Result := TYookassaReceiptResponse.Create(aRaw, True);
end;

function TYookassaCreateReceiptRequest.GetEndpoint: string;
begin
  Result := FApiBaseUrl + '/receipts';
end;

function TYookassaCreateReceiptRequest.GetMethod: string;
begin
  Result := 'POST';
end;

constructor TYookassaCreateReceiptRequest.Create;
begin
  inherited Create;
  FSend := True; // by default, we send the receipt to the client
  FReceiptType := rtPayment; // by default, the payment receipt
end;

destructor TYookassaCreateReceiptRequest.Destroy;
begin
  FSettlements.Free;
  FReceipt.Free;
  inherited Destroy;
end;

function TYookassaCreateReceiptRequest.ToJSON: TJSONObject;
var
  aSettlementsArray: TJSONArray;
  aSettlement: TYookassaSettlement;
begin
  Result := TJSONObject.Create;
  try
    EYooKassaValidationError.RaiseIfNil(FReceipt, 'Receipt');
    EYooKassaValidationError.RaiseIfFalse(FReceipt.Items.Count > 0, 'Receipt must have at least one item');

    if FReceiptType = rtRefund then
      EYooKassaValidationError.RaiseIfFalse((FPaymentId <> EmptyStr) or (FRefundId <> EmptyStr),
        'For refund receipt, either PaymentId or RefundId must be specified');
    // receipt type (payment/refund)
    Result.Add('type', ReceiptTypeToString(FReceiptType));

    // whether to send the receipt
    Result.Add('send', FSend);

    // receipt data
    if Assigned(FReceipt) then
      FReceipt.AppendJSON(Result);

    // The refund receipt requires a payment_id or a refund_id.
    if FReceiptType = rtRefund then begin
      if FPaymentId <> EmptyStr then
        Result.Add('payment_id', FPaymentId)
      else if FRefundId <> EmptyStr then
        Result.Add('refund_id', FRefundId);
    end
    else
      Result.Add('payment_id', FPaymentId);

    // A list of settlings or payments
    if Assigned(FSettlements) and (FSettlements.Count > 0) then
    begin
      aSettlementsArray := TJSONArray.Create;
      for aSettlement in FSettlements do
        aSettlementsArray.Add(aSettlement.ToJSON);
      Result.Add('settlements', aSettlementsArray);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TYookassaCreateReceiptRequest.GetReceipt: TYookassaReceipt;
begin
  if not Assigned(FReceipt) then
    FReceipt := TYookassaReceipt.Create;
  Result := FReceipt;
end;

function TYookassaCreateReceiptRequest.GetSettlements: TSettlementsList;
begin
  if not Assigned(FSettlements) then
    FSettlements := TSettlementsList.Create(True); // Auto-free
  Result := FSettlements;
end;

class function TYookassaCreateReceiptRequest.CreateReceipt(const aShopId, aSecretKey: string;
  aReceipt: TYookassaReceipt; aReceiptType: TReceiptType; const aPaymentId: string;
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

end.
