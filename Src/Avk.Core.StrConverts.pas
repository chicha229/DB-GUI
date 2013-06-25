unit Avk.Core.StrConverts;

interface

function StrToFloatExp(s: string): double;
function FloatToStrExp(Value: double): string;
function StrToDateExp(Value: string): TDateTime;
function DateToStrExp(Value: TDateTime): string;
function DateToStrUs(Value: TDateTime): string;
function DateTimeToStrExp(Date: TDateTime): string;
function StrToDateTimeExp(Value: string): TDateTime;

const
  MonthNames: array[1..12] of string = (
    '€нварь',
    'февраль',
    'март',
    'апрель',
    'май',
    'июнь',
    'июль',
    'август',
    'сент€брь',
    'окт€брь',
    'но€брь',
    'декабрь'
  );

implementation

uses
  Windows, SysUtils;

var
  FS: TFormatSettings;

function StrToFloatExp(s: string): double;
var
  p: integer;
begin
  s := Trim(s);
  if s = '' then
    Result := 0
  else
  begin
    p := Pos(',', s);
    if p = 0 then
      p := Pos('.', s);
    if p > 0 then
      s[p] := FS.DecimalSeparator;
    Result := StrToFloat(s, FS);
  end;
end;

function FloatToStrExp(Value: double): string;
begin
  Result := FloatToStr(Value, FS);
end;

function StrToDateExp(Value: string): TDateTime;
begin
  Result := StrToDate(Value, FS);
end;

function DateToStrUs(Value: TDateTime): string;
var
  Y, M, D: word;
begin
  // yyyy-mm-dd
  DecodeDate(Value, Y, M, D);
  Result := Format('%.4d-%.2d-%.2d', [Y, M, D]);
end;

procedure SetupFormatSettings;
begin
{$IFDEF VER230}
  FS.Create;
{$ELSE}
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT, FS);
{$ENDIF}
  with FS do
  begin
    DateSeparator := '.';
    TimeSeparator := ':';
    CurrencyString := '';
    DecimalSeparator := '.';
    LongDateFormat := 'DD.MM.YYYY';
    LongTimeFormat := 'HH:MM:SS';
    ShortDateFormat := 'DD.MM.YYYY';
    ShortTimeFormat := 'HH:MM:SS';
  end;
end;

function DateToStrExp(Value: TDateTime): string;
begin
  Result := DateToStr(Value, FS);
end;

function DateTimeToStrExp(Date: TDateTime): string;
begin
  Result := DateTimeToStr(Date, FS);
end;

function StrToDateTimeExp(Value: string): TDateTime;
begin
  Result := StrToDateTime(Value, FS);
end;

initialization
  SetupFormatSettings;

end.
