unit yookassa_validations;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

{ Базовые классы для валидации }
type

  { TYookassaPhoneValidator }
  TYookassaPhoneValidator = class
  public
    class procedure ValidateAndNormalizePhone(var aPhone: string; const AContext: string = '');
  end;

  { TYookassaEmailValidator }
  TYookassaEmailValidator = class
  public
    class procedure ValidateAndNormalizeEmail(var aEmail: string; const AContext: string = '');
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
  Result:=EmptyStr;
  if APhone.IsEmpty then
    Exit;

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

{ Utility functions for email validation and normalization }
function NormalizeEmail(const AEmail: string): string;
begin
  if AEmail.IsEmpty then
  begin
    Result := '';
    Exit;
  end;

  // Приводим к нижнему регистру и убираем лишние пробелы
  Result := Trim(LowerCase(AEmail));
end;

function ValidateEmail(const AEmail: string): Boolean;
var
  AtPos, DotPos, i: Integer;
  LocalPart, DomainPart: string;
  c: Char;
begin
  if AEmail.IsEmpty then
    Exit(True); // Пустой email может быть валидным в некоторых контекстах

  Result := False;

  // Базовые проверки длины
  if (Length(AEmail) < 3) or (Length(AEmail) > 254) then
    Exit;

  // Поиск символа @
  AtPos := Pos('@', AEmail);
  if (AtPos <= 1) or (AtPos = Length(AEmail)) then
    Exit;

  // Проверка на единственность @
  if Pos('@', Copy(AEmail, AtPos + 1, Length(AEmail))) > 0 then
    Exit;

  // Разделение на локальную и доменную части
  LocalPart := Copy(AEmail, 1, AtPos - 1);
  DomainPart := Copy(AEmail, AtPos + 1, Length(AEmail));

  // Проверка локальной части (до @)
  if (Length(LocalPart) < 1) or (Length(LocalPart) > 64) then
    Exit;

  // Локальная часть не должна начинаться или заканчиваться точкой
  if (LocalPart[1] = '.') or (LocalPart[Length(LocalPart)] = '.') then
    Exit;

  // Проверка доменной части (после @)
  if (Length(DomainPart) < 1) or (Length(DomainPart) > 253) then
    Exit;

  // Домен должен содержать хотя бы одну точку
  DotPos := Pos('.', DomainPart);
  if DotPos <= 1 then
    Exit;

  // Домен не должен начинаться или заканчиваться точкой или дефисом
  if (DomainPart[1] in ['.', '-']) or
     (DomainPart[Length(DomainPart)] in ['.', '-']) then
    Exit;

  // Проверка допустимых символов в локальной части
  for i := 1 to Length(LocalPart) do
  begin
    c := LocalPart[i];
    if not (c in ['a'..'z', 'A'..'Z', '0'..'9', '.', '_', '-', '+']) then
      Exit;
  end;

  // Проверка допустимых символов в доменной части
  for i := 1 to Length(DomainPart) do
  begin
    c := DomainPart[i];
    if not (c in ['a'..'z', 'A'..'Z', '0'..'9', '.', '-']) then
      Exit;
  end;

  // Проверка что последняя часть домена (TLD) содержит только буквы
  i := Length(DomainPart);
  while (i > 0) and (DomainPart[i] <> '.') do
    Dec(i);

  if i > 0 then
  begin
    for i := i + 1 to Length(DomainPart) do
    begin
      c := DomainPart[i];
      if not (c in ['a'..'z', 'A'..'Z']) then
        Exit;
    end;
  end;

  Result := True;
end;

class procedure TYookassaPhoneValidator.ValidateAndNormalizePhone(var aPhone: string; const AContext: string = '');
var
  aNormalizedPhone: string;
  aErrorContext: string;
begin
  if aPhone.IsEmpty then
    Exit;

  // СНАЧАЛА нормализуем, ПОТОМ валидируем!
  aNormalizedPhone := NormalizePhone(aPhone);

  if not ValidatePhone(aNormalizedPhone) then
  begin
    if AContext <> '' then
      aErrorContext := AContext + ' phone'
    else
      aErrorContext := 'Phone';
    // В сообщении об ошибке показываем ИСХОДНОЕ значение для удобства пользователя
    raise EYooKassaValidationError.Create(Format(_ERR_INVALID_PHONE_FORMAT, [aErrorContext, aPhone]));
  end;

  aPhone := aNormalizedPhone;
end;

class procedure TYookassaEmailValidator.ValidateAndNormalizeEmail(var aEmail: string; const AContext: string = '');
var
  aNormalizedEmail: string;
  aErrorContext: string;
begin
  if aEmail.IsEmpty then
    Exit;

  aNormalizedEmail := NormalizeEmail(aEmail);

  if not ValidateEmail(aNormalizedEmail) then
  begin
    if AContext <> '' then
      aErrorContext := AContext + ' email'
    else
      aErrorContext := 'Email';
    raise EYooKassaValidationError.Create(Format(_ERR_INVALID_EMAIL_FORMAT, [aErrorContext, aEmail]));
  end;

  aEmail := aNormalizedEmail;
end;

end.
