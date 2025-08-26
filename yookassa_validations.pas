unit yookassa_validations;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

{ Базовый класс для объектов с телефонами }
type

  { TYookassaPhoneValidator }

  TYookassaPhoneValidator = class
  public
    class procedure ValidateAndNormalizePhone(var aPhone: string; const AContext: string = '');
  end;

implementation

uses
  yookassa_exceptions, yookassa_constants
  ;

{ Utility functions for phone validation and normalization }
function NormalizePhone(const APhone: string): string;
var
  i: Integer;
  c: Char;
begin
  if APhone.IsEmpty then
  begin
    Result := '';
    Exit;
  end;

  Result := '';
  // Убираем все символы кроме цифр
  for i := 1 to Length(APhone) do
  begin
    c := APhone[i];
    if c in ['0'..'9'] then
      Result := Result + c;
  end;

  // Нормализуем российские номера
  if Result.StartsWith('8') and (Length(Result) = 11) then
    Result := '7' + Copy(Result, 2, 10)  // 8XXXXXXXXXX -> 7XXXXXXXXXX
  else if APhone.StartsWith('+7') and (Length(Result) = 11) then
    Result := Result  // +7XXXXXXXXXX -> 7XXXXXXXXXX (цифры уже извлечены)
  else if APhone.StartsWith('+') then
    Result := Result; // +XXXXXXXXXXXX -> XXXXXXXXXXXX (цифры уже извлечены)
end;

function ValidatePhone(const APhone: string): Boolean;
var
  NormalizedPhone: string;
begin
  if APhone.IsEmpty then
  begin
    Result := True; // Пустой номер может быть валидным в некоторых контекстах
    Exit;
  end;

  NormalizedPhone := NormalizePhone(APhone);

  // Проверяем ITU-T E.164 format: от 7 до 15 цифр
  Result := (Length(NormalizedPhone) >= 7) and
            (Length(NormalizedPhone) <= 15) and
            (NormalizedPhone <> '') and
            (NormalizedPhone[1] in ['1'..'9']); // Не должен начинаться с 0
end;

class procedure TYookassaPhoneValidator.ValidateAndNormalizePhone(var aPhone: string; const AContext: string = '');
var
  aNormalizedPhone: string;
  aErrorContext: string;
begin
  if aPhone.IsEmpty then
    Exit;

  if not ValidatePhone(aPhone) then
  begin
    if AContext <> '' then
      aErrorContext := AContext + ' phone'
    else
      aErrorContext := 'Phone';
    raise EYooKassaValidationError.Create(Format(_ERR_INVALID_PHONE_FORMAT, [aErrorContext, aPhone]));
  end;

  aNormalizedPhone := NormalizePhone(aPhone);
  aPhone := aNormalizedPhone;
end;

end.

