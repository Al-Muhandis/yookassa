unit yookassa_responses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, yookassa_models
  ;

type
  TYookassaLogEvent = procedure(aEvent: TEventType; const Msg: string) of object;

  { TYookassaObject }
  TYookassaObject = class
  private
    FRaw: TJSONObject;
    FOwnsRaw: Boolean;
  public
    constructor Create(aRaw: TJSONObject; aOwnsRaw: Boolean);
    destructor Destroy; override;
    property Raw: TJSONObject read FRaw;
  end;

  { TYookassaResponse }
  TYookassaResponse = class(TYookassaObject)
  private
    function GetId: string; virtual; abstract;
  public
    property ID: String read GetId;
  end;

  TPaymentStatus = (
    psNone,              // Не указан  
    psUnknown,           // Не распознан (нет в этом списке статусов)
    psPending,           // Платеж создан и ожидает действий от пользователя
    psWaitingForCapture, // Платеж оплачен, деньги авторизованы и ожидают списания
    psSucceeded,         // Платеж успешно завершен
    psCanceled           // Платеж отменен
    );

  { TYookassaPaymentResponse }
  TYookassaPaymentResponse = class(TYookassaResponse) 
  private
    function GetConfirmationURL: string;
    function GetAmount: Currency;
    function GetCurrency: String;
    function GetId: string; override;
    function GetStatus: TPaymentStatus;
  public
    property ConfirmationURL: string read GetConfirmationURL;
    property Amount: Currency read GetAmount;
    property CurrencyCode: String read GetCurrency;
    property Status: TPaymentStatus read GetStatus;
  end;

  { TYookassaReceiptResponse }
  TYookassaReceiptResponse = class(TYookassaResponse)
  private
    function GetPaymentId: String;   
    function GetId: string; override;
    function GetStatus: string;
  public
    property PaymentId: string read GetPaymentId;
    property Status: String read GetStatus;
    property ID: String read GetId;
  end;

function StringToPaymentStatus(const aPaymentStatus: String): TPaymentStatus;

implementation

uses
  opensslsockets, yookassa_constants
  ;

function StringToPaymentStatus(const aPaymentStatus: String): TPaymentStatus;
begin
  case aPaymentStatus of
    '':                                  Result:=psNone;
    _PAYMENT_STATUS_PENDING:             Result:=psPending;
    _PAYMENT_STATUS_WAITING_FOR_CAPTURE: Result:=psWaitingForCapture;
    _PAYMENT_STATUS_SUCCEEDED:           Result:=psSucceeded;
    _PAYMENT_STATUS_CANCELED:            Result:=psCanceled;
  else
    Result:=psUnknown;
  end;
end;

{ TYookassaObject }

constructor TYookassaObject.Create(aRaw: TJSONObject; aOwnsRaw: Boolean);
begin
  FRaw:=ARaw;
  FOwnsRaw:=aOwnsRaw;
end;

destructor TYookassaObject.Destroy;
begin
  if FOwnsRaw then
    FRaw.Free;
  inherited Destroy;
end;

{ TYookassaPaymentResponse }

function TYookassaPaymentResponse.GetId: string;
begin
  Result := Raw.Get(_JSON_FIELD_ID, EmptyStr);
end;

function TYookassaPaymentResponse.GetStatus: TPaymentStatus;
begin
  Result := StringToPaymentStatus(Raw.Get(_JSON_FIELD_STATUS, EmptyStr));
end;

function TYookassaPaymentResponse.GetConfirmationURL: string;
begin
  if Assigned(Raw.Find(_JSON_FIELD_CONFIRMATION)) then
    Result := Raw.Objects[_JSON_FIELD_CONFIRMATION].Get(_JSON_FIELD_CONFIRMATION_URL, EmptyStr)
  else
    Result := EmptyStr;
end;

function TYookassaPaymentResponse.GetAmount: Currency;
var
  aValueStr: string;
begin
  Result := 0;
  aValueStr := Raw.FindPath(_JSON_FIELD_AMOUNT + '.' + _JSON_FIELD_VALUE).AsString;
  if not aValueStr.IsEmpty then
    Result := StrToCurr(aValueStr, _FrmtStngsJSON);
end;

function TYookassaPaymentResponse.GetCurrency: String;
begin
  Result := Raw.FindPath(_JSON_FIELD_AMOUNT + '.' + _JSON_FIELD_CURRENCY).AsString;
end;

{ TYookassaReceiptResponse }

function TYookassaReceiptResponse.GetPaymentId: String;
begin
  Result := Raw.Get(_JSON_FIELD_PAYMENT_ID, EmptyStr);
end;

function TYookassaReceiptResponse.GetId: string;
begin
  Result := Raw.Get(_JSON_FIELD_ID, EmptyStr);
end;

function TYookassaReceiptResponse.GetStatus: string;
begin
  Result := Raw.Get(_JSON_FIELD_STATUS, EmptyStr);
end;

end.
