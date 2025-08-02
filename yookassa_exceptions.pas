unit yookassa_exceptions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson
  ;

type
  { Exception for the YooKassa API errors (responses with code >= 400) }
  EYooKassaError = class(Exception)
  private
    FStatusCode: Integer;
    FErrorJSON: TJSONObject;
    function GetDescription: string;
    function GetErrorCode: string;
  public
    constructor CreateFromResponse(const AStatusCode: Integer; AErrorJSON: TJSONObject);
    destructor Destroy; override;
    property StatusCode: Integer read FStatusCode;
    property ErrorJSON: TJSONObject read FErrorJSON;
    property ErrorCode: string read GetErrorCode;
    property Description: string read GetDescription;
  end;

  { Exception for data validation errors before sending the request }
  EYooKassaValidationError = class(Exception)
  public
    constructor Create(const Msg: string);
    class procedure RaiseIfEmpty(const Value, FieldName: string);
    class procedure RaiseIfZeroOrNegative(const Value: Double; const FieldName: string);
    class procedure RaiseIfNil(const Value: TObject; const FieldName: string);
    class procedure RaiseIfFalse(const Condition: Boolean; const Msg: string);
  end;

implementation

{ EYooKassaError }

constructor EYooKassaError.CreateFromResponse(const AStatusCode: Integer; AErrorJSON: TJSONObject);
begin
  inherited Create('');
  FStatusCode := AStatusCode;
  FErrorJSON := AErrorJSON;
  if Assigned(AErrorJSON) then
    Message := Format('YooKassa API error [%d]: %s', [AStatusCode, GetDescription])
  else
    Message := Format('YooKassa API error [%d]: Unknown error', [AStatusCode]);
end;

destructor EYooKassaError.Destroy;
begin
  FErrorJSON.Free;
  inherited Destroy;
end;

function EYooKassaError.GetDescription: string;
begin
  if Assigned(FErrorJSON) then
    Result := FErrorJSON.Get('description', 'No description')
  else
    Result := 'No error JSON available';
end;

function EYooKassaError.GetErrorCode: string;
begin
  if Assigned(FErrorJSON) then
    Result := FErrorJSON.Get('code', 'unknown')
  else
    Result := 'unknown';
end;

{ EYooKassaValidationError }

constructor EYooKassaValidationError.Create(const Msg: string);
begin
  inherited Create('Validation failed: ' + Msg);
end;

class procedure EYooKassaValidationError.RaiseIfEmpty(const Value, FieldName: string);
begin
  if Value = '' then
    raise Self.Create(FieldName + ' is required');
end;

class procedure EYooKassaValidationError.RaiseIfZeroOrNegative(const Value: Double; const FieldName: string);
begin
  if Value <= 0 then
    raise Self.Create(FieldName + ' must be greater than zero');
end;

class procedure EYooKassaValidationError.RaiseIfNil(const Value: TObject; const FieldName: string);
begin
  if not Assigned(Value) then
    raise Self.Create(FieldName + ' is required');
end;

class procedure EYooKassaValidationError.RaiseIfFalse(const Condition: Boolean; const Msg: string);
begin
  if not Condition then
    raise Self.Create(Msg);
end;

end.
