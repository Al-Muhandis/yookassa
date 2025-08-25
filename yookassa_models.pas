unit yookassa_models;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fgl
  ;

type

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

  { Признак способа расчета передается в параметре payment_mode }
  TPaymentMode = (
    pmNone,
    pmFullPrepayment,   // Полная предоплата
    pmFullPayment       // Полный расчет
  );

  { TYookassaReceiverType }
  TYookassaReceiverType = (
    rtUnknown,
    rtBankAccount,
    rtMobileBalance,
    rtDigitalWallet
  );

  TReceiptType = (
    rtNone,     // Неопределен
    rtPayment,  // Приход
    rtRefund    // Возврат прихода
  );

  TSettlementType = (
    stNone,             // Не указан
    stCashless,         // Безналичный расчет
    stPrepayment,       //  Предоплата (аванс)
    stPostpayment,      // Постоплата (кредит)
    stConsideration     // Встречное предоставление
  );

  TYookassaReceiptItem = class; 
  TYookassaSettlement = class;

  // Collections
  TReceiptItems = specialize TFPGObjectList<TYookassaReceiptItem>;
  TSettlementsList = specialize TFPGObjectList<TYookassaSettlement>;

  TYookassaAPIObject = class
  public
    function ToJSON: TJSONObject; virtual; abstract;
  end;

  // Модель поставщика
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

  { TYookassaReceiptItem }
  TYookassaReceiptItem = class(TYookassaAPIObject)
  private
    FAgentType: TYookassaAgentType;
    FPaymentMode: TPaymentMode;
    FSupplier: TYookassaSupplier;
    function GetAgentTypeString: string;
    function GetSupplier: TYookassaSupplier;
  public
    Description: string;
    Quantity: Double;
    AmountValue: Currency;
    AmountCurrency: string;
    VatCode: Integer;
    PaymentSubject: string;
    MarkMode: Integer;
    MarkCodeInfo: string; // base64 gs_1m,
    Measure: string;
    constructor Create;
    destructor Destroy; override;
    function ToJSON: TJSONObject; override;
    property AgentType: TYookassaAgentType read FAgentType write FAgentType;
    property PaymentMode: TPaymentMode read FPaymentMode write FPaymentMode;
    property Supplier: TYookassaSupplier read GetSupplier;
  end;


  { TYookassaUser }
  TYookassaUser = class(TYookassaAPIObject)
  private
    FEmail: String;
    FFullName: String;
    FINN: String;
    FPhone: String;
  public
    function ToJSON: TJSONObject; override;
{ Для юрлица — название организации, для ИП и физического лица — ФИО. Если у физлица отсутствует ИНН,
  в этом же параметре передаются паспортные данные. Не более 256 символов. }
    property FullName: String read FFullName write FFullName;
{ ИНН пользователя (10 или 12 цифр).
  Если у физического лица отсутствует ИНН, необходимо передать паспортные данные в параметре full_name }
    property INN: String read FINN write FINN;
{ Электронная почта пользователя для отправки чека. Обязательный параметр, если используете Чеки от ЮKassa
  или если используете другое решение (стороннюю онлайн-кассу, чеки самозанятых) и не передаете phone. }
    property Email: String read FEmail write FEmail;
{ Телефон пользователя для отправки чека. Указывается в формате ITU-T E.164, например 79000000000.
  Обязательный параметр, если не передан email }
    property Phone: String read FPhone write FPhone;
  end;

  // Модель чека
  { TYookassaReceipt }
  TYookassaReceipt = class(TYookassaAPIObject)
  private
    FCustomer: TYookassaUser;
    FItems: TReceiptItems;
    FTaxSystemCode: Integer;
    function GetCustomer: TYookassaUser;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddItem(aItem: TYookassaReceiptItem);
    function ToJSON: TJSONObject;  override;
    procedure AppendJSON(aJSON: TJSONObject);
    property Customer: TYookassaUser read GetCustomer write FCustomer;
    property Items: TReceiptItems read FItems write FItems;
    property TaxSystemCode: Integer read FTaxSystemCode write FTaxSystemCode;
  end;

  // Модель получателя платежа
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

  // Модель расчета
  { TYookassaSettlement }
  TYookassaSettlement = class(TYookassaAPIObject)
  private
    FType: TSettlementType;
    FAmountValue: Currency;
    FAmountCurrency: string;
  public
    constructor Create; overload;
    constructor Create(aType: TSettlementType; aAmount: Currency; const aCurrency: string); overload;
    function ToJSON: TJSONObject; override;
    property SettlementType: TSettlementType read FType write FType;
    property Amount: Currency read FAmountValue write FAmountValue;
    property Currency: string read FAmountCurrency write FAmountCurrency;
  end;

// Утилитарные функции для работы с перечислениями
function PaymentModeToString(aPaymentMode: TPaymentMode): String;
function ReceiptTypeToString(aType: TReceiptType): String;
function SettlementTypeToString(aSettlementType: TSettlementType): String;
function IsValidBase64(const AStr: string): Boolean;

var
  _FrmtStngsJSON: TFormatSettings;

implementation

uses
  base64, yookassa_exceptions
  ;

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

function PaymentModeToString(aPaymentMode: TPaymentMode): String;
begin
  case aPaymentMode of
    pmFullPrepayment: Result:='full_prepayment';
    pmFullPayment:    Result:='full_payment';
  else
    Result:=EmptyStr;
  end;
end;

function ReceiptTypeToString(aType: TReceiptType): String;
begin
  case aType of
    rtPayment: Result:='payment';
    rtRefund:  Result:='refund';
  else
    Result:=EmptyStr;
  end;
end;

function SettlementTypeToString(aSettlementType: TSettlementType): String;
begin
  case aSettlementType of
    stCashless:      Result:='cashless';
    stPrepayment:    Result:='prepayment';
    stPostpayment:   Result:='postpayment';
    stConsideration: Result:='consideration';
  else
    Result:=EmptyStr;
  end;
end;

{ TYookassaReceipt }

function TYookassaReceipt.GetCustomer: TYookassaUser;
begin
  if not Assigned(FCustomer) then
    FCustomer:=TYookassaUser.Create;
  Result:=FCustomer;
end;

constructor TYookassaReceipt.Create;
begin
  Items := TReceiptItems.Create(True);
  FTaxSystemCode := -1; // not specified
end;

destructor TYookassaReceipt.Destroy;
begin
  FCustomer.Free;
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
  Item: TYookassaReceiptItem;
begin
  if Assigned(FCustomer) then
    if (FCustomer.Email <> EmptyStr) or (FCustomer.Phone <> EmptyStr) then
      aJSON.Add('customer', FCustomer.ToJSON);

  // items
  aItems := TJSONArray.Create;
  for Item in FItems do
    aItems.Add(Item.ToJSON);
  aJson.Add('items', aItems);

  // tax_system_code
  if FTaxSystemCode >= 0 then
    aJson.Add('tax_system_code', FTaxSystemCode);
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

{ TYookassaReceiptItem }

function TYookassaReceiptItem.GetAgentTypeString: string;
begin
  case FAgentType of
    atBankingPaymentAgent:    Result := 'banking_payment_agent';
    atBankingPaymentSubagent: Result := 'banking_payment_subagent';
    atPaymentAgent:           Result := 'payment_agent';
    atPaymentSubagent:        Result := 'payment_subagent';
    atAttorney:               Result := 'attorney';
    atCommissioner:           Result := 'commissioner';
    atAgent:                  Result := 'agent';
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
  inherited Create;
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
    if FPaymentMode <> pmNone then Result.Add('payment_mode', PaymentModeToString(FPaymentMode));
    if PaymentSubject <> EmptyStr then Result.Add('payment_subject', PaymentSubject);
    if MarkMode >= 0 then
    begin
      Result.Add('mark_mode', MarkMode);

      // check MarkCodeInfo
      if MarkCodeInfo = EmptyStr then
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

    if Measure <> EmptyStr then
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

{ TYookassaSettlement }

constructor TYookassaSettlement.Create;
begin
  FAmountCurrency := 'RUB';
end;

constructor TYookassaSettlement.Create(aType: TSettlementType; aAmount: Currency; const aCurrency: string);
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
    if FType <> stNone then
      Result.Add('type', SettlementTypeToString(FType));

    aAmount := TJSONObject.Create;
    aAmount.Add('value', Format('%.2f', [FAmountValue], _FrmtStngsJSON));
    aAmount.Add('currency', FAmountCurrency);
    Result.Add('amount', aAmount);
  except
    FreeAndNil(Result);
    raise;
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
    if FName <> EmptyStr then Result.Add('name', FName);
    if FPhone <> EmptyStr then Result.Add('phone', FPhone);
    if FInn <> EmptyStr then Result.Add('inn', FInn);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TYookassaUser }

function TYookassaUser.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    if not FFullName.IsEmpty then
      Result.Add('full_name', FFullName);
    if not FINN.IsEmpty then
      Result.Add('inn', FINN);
    if not FEmail.IsEmpty then
      Result.Add('email', FEmail);
    if not FPhone.IsEmpty then
      Result.Add('phone', FPhone);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

initialization
  _FrmtStngsJSON := DefaultFormatSettings;
  _FrmtStngsJSON.DecimalSeparator := '.';

end.
