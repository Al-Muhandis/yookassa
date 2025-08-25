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
  public
    constructor Create(aRaw: TJSONObject);
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
    property Currency: String read GetCurrency;
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
  opensslsockets
  ;

function StringToPaymentStatus(const aPaymentStatus: String): TPaymentStatus;
begin
  case aPaymentStatus of
    '':                    Result:=psNone;
    'pending':             Result:=psPending;
    'waiting_for_capture': Result:=psWaitingForCapture;
    'succeeded':           Result:=psSucceeded;
    'canceled':            Result:=psCanceled;
  else
    Result:=psUnknown;
  end;
end;

{ TYookassaObject }

constructor TYookassaObject.Create(aRaw: TJSONObject);
begin
  FRaw:=ARaw;
end;

destructor TYookassaObject.Destroy;
begin
  FRaw.Free;
  inherited Destroy;
end;

{ TYookassaPaymentResponse }

function TYookassaPaymentResponse.GetId: string;
begin
  Result := Raw.Get('id', EmptyStr);
end;

function TYookassaPaymentResponse.GetStatus: TPaymentStatus;
begin
  Result := StringToPaymentStatus(Raw.Get('status', EmptyStr));
end;

function TYookassaPaymentResponse.GetConfirmationURL: string;
begin
  if Assigned(Raw.Find('confirmation')) then
    Result := Raw.Objects['confirmation'].Get('confirmation_url', EmptyStr)
  else
    Result := EmptyStr;
end;

function TYookassaPaymentResponse.GetAmount: Currency;
var
  aValueStr: string;
begin
  Result := 0;
  aValueStr := Raw.FindPath('amount.value').AsString;
  if not aValueStr.IsEmpty then
    Result := StrToCurr(aValueStr, _FrmtStngsJSON);
end;

function TYookassaPaymentResponse.GetCurrency: String;
begin
  Result := Raw.FindPath('amount.currency').AsString;
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
  Result := Raw.Get('status', EmptyStr);
end;

end.
