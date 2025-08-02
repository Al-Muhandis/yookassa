unit yookassa_exceptions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson
  ;

type
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

implementation

{ EYooKassaError }

constructor EYooKassaError.CreateFromResponse(const AStatusCode: Integer; AErrorJSON: TJSONObject);
begin
  inherited Create('');
  FStatusCode := AStatusCode;
  FErrorJSON := AErrorJSON; // владеет вызывающий код? Нет — делаем копию
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

end.

