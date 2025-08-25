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
  TYookassaReceiver = class(TYookassaAPIObject)
  private
    FReceiverType: TYookassaReceiverType;
    FAccountNumber: string;
    FBic: string;
    FPhone: string;
    function GetTypeString: string;
  public
    constructor Create; overload;
    constructor Create(aType: TYookassaReceiverType); overload;
    function ToJSON: TJSONObject; override;
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
  base64, yookassa_exceptions, yookassa_constants
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
    pmFullPrepayment: Result:=_PAYMENT_MODE_FULL_PREPAYMENT;
    pmFullPayment:    Result:=_PAYMENT_MODE_FULL_PAYMENT;
  else
    Result:=EmptyStr;
  end;
end;

function ReceiptTypeToString(aType: TReceiptType): String;
begin
  case aType of
    rtPayment: Result:=_RECEIPT_TYPE_PAYMENT;
    rtRefund:  Result:=_RECEIPT_TYPE_REFUND;
  else
    Result:=EmptyStr;
  end;
end;

function SettlementTypeToString(aSettlementType: TSettlementType): String;
begin
  case aSettlementType of
    stCashless:      Result:=_SETTLEMENT_TYPE_CASHLESS;
    stPrepayment:    Result:=_SETTLEMENT_TYPE_PREPAYMENT;
    stPostpayment:   Result:=_SETTLEMENT_TYPE_POSTPAYMENT;
    stConsideration: Result:=_SETTLEMENT_TYPE_CONSIDERATION;
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
  FTaxSystemCode := _DEFAULT_TAX_SYSTEM_CODE; // not specified
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
      aJSON.Add(_JSON_FIELD_CUSTOMER, FCustomer.ToJSON);

  // items
  aItems := TJSONArray.Create;
  for Item in FItems do
    aItems.Add(Item.ToJSON);
  aJson.Add(_JSON_FIELD_ITEMS, aItems);

  // tax_system_code
  if FTaxSystemCode >= 0 then
    aJson.Add(_JSON_FIELD_TAX_SYSTEM_CODE, FTaxSystemCode);
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
    rtBankAccount: Result := _RECEIVER_TYPE_BANK_ACCOUNT;
    rtMobileBalance: Result := _RECEIVER_TYPE_MOBILE_BALANCE;
    rtDigitalWallet: Result := _RECEIVER_TYPE_DIGITAL_WALLET;
  else
    Result := EmptyStr;
  end;
end;

function TYookassaReceiver.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.Add(_JSON_FIELD_TYPE, GetTypeString);

    case FReceiverType of
      rtBankAccount:
        begin
          EYooKassaValidationError.RaiseIfEmpty(FAccountNumber, 'AccountNumber');
          EYooKassaValidationError.RaiseIfEmpty(FBic, 'Bic');
          Result.Add(_JSON_FIELD_ACCOUNT_NUMBER, FAccountNumber);
          Result.Add(_JSON_FIELD_BIC, FBic);
        end;
      rtMobileBalance:
        begin
          EYooKassaValidationError.RaiseIfEmpty(FPhone, 'Phone');
          Result.Add(_JSON_FIELD_PHONE, FPhone);
        end;
      rtDigitalWallet:
        begin
          EYooKassaValidationError.RaiseIfEmpty(FAccountNumber, 'AccountNumber');
          Result.Add(_JSON_FIELD_ACCOUNT_NUMBER, FAccountNumber);
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
    atBankingPaymentAgent:    Result := _AGENT_TYPE_BANKING_PAYMENT_AGENT;
    atBankingPaymentSubagent: Result := _AGENT_TYPE_BANKING_PAYMENT_SUBAGENT;
    atPaymentAgent:           Result := _AGENT_TYPE_PAYMENT_AGENT;
    atPaymentSubagent:        Result := _AGENT_TYPE_PAYMENT_SUBAGENT;
    atAttorney:               Result := _AGENT_TYPE_ATTORNEY;
    atCommissioner:           Result := _AGENT_TYPE_COMMISSIONER;
    atAgent:                  Result := _AGENT_TYPE_AGENT;
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
  MarkMode := _DEFAULT_MARK_MODE; // -1 = not specified
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
    Result.Add(_JSON_FIELD_DESCRIPTION, Description);
    Result.Add(_JSON_FIELD_QUANTITY, Quantity);
    aAmount := TJSONObject.Create;
    aAmount.Add(_JSON_FIELD_VALUE, Format('%.2f', [AmountValue], _FrmtStngsJSON));
    aAmount.Add(_JSON_FIELD_CURRENCY, AmountCurrency);
    Result.Add(_JSON_FIELD_AMOUNT, aAmount);
    Result.Add(_JSON_FIELD_VAT_CODE, VatCode);
    if FPaymentMode <> pmNone then Result.Add(_JSON_FIELD_PAYMENT_MODE, PaymentModeToString(FPaymentMode));
    if PaymentSubject <> EmptyStr then Result.Add(_JSON_FIELD_PAYMENT_SUBJECT, PaymentSubject);
    if MarkMode >= 0 then
    begin
      Result.Add(_JSON_FIELD_MARK_MODE, MarkMode);

      // check MarkCodeInfo
      if MarkCodeInfo = EmptyStr then
        raise EYooKassaValidationError.Create(_ERR_MARK_CODE_REQUIRED);

      if not IsValidBase64(MarkCodeInfo) then
        raise EYooKassaValidationError.Create(Format(_ERR_INVALID_BASE64, ['MarkCodeInfo']));

      // Optional: length check after decoding
      try
        aMarkCodeBytes := DecodeStringBase64(MarkCodeInfo);
        if (Length(aMarkCodeBytes) < _MIN_MARK_CODE_LENGTH) or (Length(aMarkCodeBytes) > _MAX_MARK_CODE_LENGTH) then
          raise EYooKassaValidationError.Create(
            Format(_ERR_INVALID_MARK_CODE_LENGTH, ['MarkCodeInfo', _MIN_MARK_CODE_LENGTH, _MAX_MARK_CODE_LENGTH]));
      except
        on E: Exception do
          raise EYooKassaValidationError.Create(Format(_ERR_MARK_CODE_DECODE, [E.Message]));
      end;

      // add to JSON
      aMarkCodeInfo := TJSONObject.Create;
      aMarkCodeInfo.Add(_JSON_FIELD_GS_1M, MarkCodeInfo);
      Result.Add(_JSON_FIELD_MARK_CODE_INFO, aMarkCodeInfo);
    end;

    if Measure <> EmptyStr then
      Result.Add(_JSON_FIELD_MEASURE, Measure);

    // agent_type (ФФД 1.1)
    aAgentTypeStr := GetAgentTypeString;
    if aAgentTypeStr <> EmptyStr then
      Result.Add(_JSON_FIELD_AGENT_TYPE, aAgentTypeStr);

    if Assigned(FSupplier) then
      Result.Add(_JSON_FIELD_SUPPLIER, FSupplier.ToJSON);

  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TYookassaSettlement }

constructor TYookassaSettlement.Create;
begin
  FAmountCurrency := _DEFAULT_CURRENCY;
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
      Result.Add(_JSON_FIELD_TYPE, SettlementTypeToString(FType));

    aAmount := TJSONObject.Create;
    aAmount.Add(_JSON_FIELD_VALUE, Format('%.2f', [FAmountValue], _FrmtStngsJSON));
    aAmount.Add(_JSON_FIELD_CURRENCY, FAmountCurrency);
    Result.Add(_JSON_FIELD_AMOUNT, aAmount);
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
    if FName <> EmptyStr then Result.Add(_JSON_FIELD_NAME, FName);
    if FPhone <> EmptyStr then Result.Add(_JSON_FIELD_PHONE, FPhone);
    if FInn <> EmptyStr then Result.Add(_JSON_FIELD_INN, FInn);
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
      Result.Add(_JSON_FIELD_FULL_NAME, FFullName);
    if not FINN.IsEmpty then
      Result.Add(_JSON_FIELD_INN, FINN);
    if not FEmail.IsEmpty then
      Result.Add(_JSON_FIELD_EMAIL, FEmail);
    if not FPhone.IsEmpty then
      Result.Add(_JSON_FIELD_PHONE, FPhone);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

initialization
  _FrmtStngsJSON := DefaultFormatSettings;
  _FrmtStngsJSON.DecimalSeparator := '.';

end.
