unit AVK.Core.Utils;

interface

uses
  Classes, DB, Forms, Controls, Types;

function DotStrToFloat(s: string): double;

function Confirm(const Msg: string): boolean;
function ConfirmYNC(const Msg: string): TModalResult;

function  RectToString(Rect: TRect): string;
function  StringToRect(Value: string): TRect;
function  SettingsFileName: string;
procedure SaveSettingsValue(Section: string; Name: string; Value: string);
function  LoadSettingsValue(Section: string; Name: string): string;
procedure LoadFormSettings(AForm: TForm);
procedure SaveFormSettings(AForm: TForm);
function  ShellOpenFile(FileName: string): boolean;
function  DelimitedConcat(s1, s2, Delimiter: string): string;
procedure SplitDelimitedString(AStrings: TStrings; AText, ADelimiter: string);
function  MergeDelimitedString(AStrings: TStrings; ADelimiter: string): string;
procedure ShowStrings(ACaption: string; Strings: TStrings);
procedure SendMail(
  RecipientName, RecipientAddress, MsgSubject, MsgText: AnsiString;
  FileNames: TStrings = nil);
  {
function  DateToStrExp(Date: TDate): string;
function  DateTimeToStrExp(Date: TDate): string;
function  StrToDateExp(Value: string): TDate;
function  DotFloatToStr(Value: Double): string;
}
function  GetExeVersionString: string;
function  IIF(Condition: boolean; TrueValue: string; FalseValue: string): string; overload;
function  IIF(Condition: boolean; TrueValue: TObject; FalseValue: TObject): TObject; overload;
function  IIF(Condition: boolean; TrueValue: double; FalseValue: double): double; overload;
function  IIF(Condition: boolean; TrueValue, FalseValue: integer): integer; overload;
procedure DisableProcessWindowsGhosting;
//function  GetStringHash(Str: string): string;
function  MonthName(Month: integer): string;
function  GetTempPathWrapper: string;
function  FileGetTempName(const Prefix: string): string;
function  GetApplicationDataFolder: string;
function  InputString(const Prompt: string; var Value: string): boolean;
function  IsNumber(const Value: string): boolean;
function  IsInteger(const Value: string): boolean;
function  GetComputerNetName: string;
function VarArraysIsEqual(const A, B: variant): boolean;
function VarsIsEqual(const A, B: variant): boolean;

var
  UseExeFolderForIni: boolean = false;

implementation

uses
  Windows, SysUtils, IniFiles, Dialogs, ShellApi,
  StdCtrls, Mapi, ShlObj, Variants;

const
  FormBoundsSettingsName = 'Bounds';

  MonthNames: array[1..12] of string = (
    'январь',
    'февраль',
    'март',
    'апрель',
    'май',
    'июнь',
    'июль',
    'август',
    'сентябрь',
    'октябрь',
    'ноябрь',
    'декабрь'
  );

var
  LSettingsFileName: string;
  
function DotStrToFloat(s: string): double;
var
  p: integer;
  ss: string;
begin
  ss := Trim(s);
  if ss = '' then
    Result := 0
  else
  begin
    p := Pos(',', ss);
    if p = 0 then
      p := Pos('.', ss);
    if p > 0 then
      ss[p] := FormatSettings.DecimalSeparator;
    Result := StrToFloat(ss);
  end;
end;

function Confirm(const Msg: string): boolean;
begin
  Result := Application.MessageBox(
    PChar(Msg),
    'Подтвердите',
    MB_ICONQUESTION or MB_YESNO
  ) = IDYES;
end;

function ConfirmYNC(const Msg: string): TModalResult;
begin
  Result := Application.MessageBox(
    PChar(Msg),
    'Подтвердите',
    MB_ICONQUESTION or MB_YESNOCANCEL
  );
end;

function  SettingsFileName: string;
var
  ExeFileName: string;
begin
  if LSettingsFileName = '' then
  begin
    if UseExeFolderForIni then
      LSettingsFileName := ChangeFileExt(Application.ExeName, '.ini')
    else
    begin
      ExeFileName := ChangeFileExt(ExtractFileName(Application.ExeName), '');
      LSettingsFileName :=
        GetApplicationDataFolder +
        ExeFileName + '\' +
        ExeFileName + '.ini';
    end
  end;
  Result := LSettingsFileName;
end;

function  RectToString(Rect: TRect): string;
begin
  Result :=
    IntToStr(Rect.Left) + ',' +
    IntToStr(Rect.Top) + ',' +
    IntToStr(Rect.Right) + ',' +
    IntToStr(Rect.Bottom);
end;

function  StringToRect(Value: string): TRect;
begin
  with TStringList.Create do
    try
      Delimiter := ',';
      DelimitedText := Value;
      Result.Left := StrToInt(Strings[0]);
      Result.Top := StrToInt(Strings[1]);
      Result.Right := StrToInt(Strings[2]);
      Result.Bottom := StrToInt(Strings[3]);
    finally
      Free;
    end;
end;

procedure EnsureSettingsFolderExists;
var
  D: string;
begin
  D := ExtractFilePath(SettingsFileName);
  if not DirectoryExists(D) then
    CreateDir(D);
end;

procedure SaveSettingsValue(Section: string; Name: string; Value: string);
begin
  EnsureSettingsFolderExists;
  with TIniFile.Create(SettingsFileName) do
    try
      WriteString(Section, Name, Value);
    finally
      Free;
    end;
end;

function  LoadSettingsValue(Section: string; Name: string): string;
var
  Buffer: array[0..16383] of Char;
begin
  EnsureSettingsFolderExists;
  with TIniFile.Create(SettingsFileName) do
    try
      //Result := ReadString(Section, Name, '');
      SetString(Result, Buffer, GetPrivateProfileString(PChar(Section),
        PChar(Name), PChar(''), Buffer, SizeOf(Buffer), PChar(FileName)));
    finally
      Free;
    end;
end;

procedure LoadFormSettings(AForm: TForm);
var
//  i: integer;
  Str: string;
begin
  Str := LoadSettingsValue(
    AForm.ClassName, FormBoundsSettingsName);
  if Str <> '' then
    AForm.BoundsRect := StringToRect(Str);
{
  for i := 0 to AForm.ComponentCount - 1 do
    if AForm.Components[i] is TdxBarManager then
    begin
      (AForm.Components[i] as TdxBarManager).
        LoadFromIniFile(SettingsFileName);
      Break;
    end;
}
end;

procedure SaveFormSettings(AForm: TForm);
{
var
  i: integer;
}
begin
  SaveSettingsValue(
    AForm.ClassName,
    FormBoundsSettingsName,
    RectToString(AForm.BoundsRect)
  );
{
  for i := 0 to AForm.ComponentCount - 1 do
    if AForm.Components[i] is TdxBarManager then
    begin
      (AForm.Components[i] as TdxBarManager).
        SaveToIniFile(SettingsFileName);
      Break;
    end;
}
end;

function  ShellOpenFile(FileName: string): boolean;
begin
  Result := ShellExecute(
    0,
    'open',
    PChar(FileName),
    nil,
    nil,
    SW_SHOW
  ) > 32;
end;

function  DelimitedConcat(s1, s2, Delimiter: string): string;
begin
  if s1 = '' then
    Result := s2
  else if s2 = '' then
    Result := s1
  else
    Result := s1 + Delimiter + s2;
end;

procedure SplitDelimitedString(AStrings: TStrings; AText, ADelimiter: string);
var
  p, n: integer;
  Text: PChar;
begin
  Text := PChar(AText);
  AStrings.Clear;
  n := Length(ADelimiter);
  while Assigned(Text) do
  begin
    p := Pos(ADelimiter, Text) - 1;
    if p < 0 then
      Break;
    AStrings.Add(Copy(Text, 1, p));
    Inc(Text, p + n);
  end;
  if Assigned(Text) and (Length(Text) > 0) then
    AStrings.Add(Text);
end;

function  MergeDelimitedString(AStrings: TStrings; ADelimiter: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to AStrings.Count - 1 do
    Result := DelimitedConcat(Result, AStrings[i], ADelimiter);
end;

procedure ShowStrings(ACaption: string; Strings: TStrings);
var
  F: TForm;
begin
  F := TForm.Create(Application);
  with F do
    try
      Caption := ACaption;
      Position := poScreenCenter;
      with TMemo.Create(F) do
      begin
        Parent := F;
        Align := alClient;
        Lines := Strings;
      end;
      ShowModal;
    finally
      Free;
    end;
end;

procedure SendMail(
  RecipientName, RecipientAddress, MsgSubject, MsgText: AnsiString;
  FileNames: TStrings);
var
  i: integer;
  Msg: TMapiMessage;
  Recipient: TMapiRecipDesc;
  Files: array of TMapiFileDesc;
  SendResult: cardinal;
begin
  Recipient.ulReserved := 0;
  Recipient.ulRecipClass := MAPI_TO;
  Recipient.lpszName := PAnsiChar(RecipientName);
  Recipient.lpszAddress := PAnsiChar(RecipientAddress);
  Recipient.ulEIDSize := 0;
  Recipient.lpEntryID := nil;

  Msg.ulReserved := 0;
  Msg.lpszSubject := PAnsiChar(MsgSubject);
  Msg.lpszNoteText := PAnsiChar(MsgText);
  Msg.lpszMessageType := nil;
  Msg.lpszDateReceived := PAnsiChar(AnsiString(FormatDateTime('yyyy/mm/dd hh:nn', Now)));
  Msg.flFlags := MAPI_UNREAD;
  Msg.lpOriginator := nil;
  Msg.nRecipCount := 1;
  Msg.lpRecips := @Recipient;
  if not Assigned(FileNames) then
  begin
    Msg.nFileCount := 0;
    Msg.lpFiles := nil;
  end
  else
  begin
    SetLength(Files, FileNames.Count);
    for I := 0 to FileNames.Count - 1 do
    begin
      {if not FileExists(FileNames[I]) then
        continue;}
      FileNames[I] := ExpandFileName(FileNames[I]);
      FillChar(Files[I], SizeOf(TMapiFileDesc), #0);
      Files[I].nPosition := DWORD(-1);
      Files[I].lpszPathName := PAnsiChar(AnsiString(FileNames[I]));
    end;
    Msg.nFileCount := FileNames.Count;
    Msg.lpFiles := PMapiFileDesc(Files);
  end;

  SendResult := MapiSendMail(
    0, Application.Handle, Msg,
    MAPI_DIALOG or MAPI_LOGON_UI or MAPI_NEW_SESSION, 0);
  if (SendResult <> SUCCESS_SUCCESS) and
     (SendResult <> MAPI_USER_ABORT) then
    raise Exception.CreateFmt('Ошибка создания письма: %d', [SendResult]);
end;

{
function DateToStrExp(Date: TDate): string;
var
  Y, M, D: word;
begin
  DecodeDate(Date, Y, M, D);
  Result := Format('%.2d.%.2d.%.4d', [D, M, Y]);
end;

function DateTimeToStrExp(Date: TDate): string;
var
  Y, M, D: word;
  Hour, Min, Sec, MSec: word;
begin
  DecodeDate(Date, Y, M, D);
  DecodeTime(Date, Hour, Min, Sec, MSec);
  Result := Format('%.2d.%.2d.%.4d %.2d:%.2d:%.2d', [D, M, Y, Hour, Min, Sec]);
end;

function  StrToDateExp(Value: string): TDate;
begin
//  12.45.7890
  Result := EncodeDate(
    StrToInt(Copy(Value, 7, 4)),
    StrToInt(Copy(Value, 4, 2)),
    StrToInt(Copy(Value, 1, 2))
  );
end;


function DotFloatToStr(Value: double): string;
var
  FS: TFormatSettings;
begin
  FS.Create;
  FS.DecimalSeparator := '.';
  Result := FloatToStr(Value, FS);
end;
}

function GetExeVersionString: string;
var
  Info: Pointer;
  FileInfo: PVSFixedFileInfo;
  InfoSize, FileSize: cardinal;
  Major1, Major2, Minor1, Minor2: Integer;
begin
  try
    InfoSize := GetFileVersionInfoSize(pChar(Application.ExeName), FileSize);
    if InfoSize = 0 then
      raise Exception.Create('Error retrieving version info size.');
    GetMem(Info, InfoSize);
    try
      GetFileVersionInfo(PChar(Application.ExeName), 0, InfoSize, Info);
      VerQueryValue(Info, '\', Pointer(FileInfo), FileSize);
      Major1 := FileInfo.dwFileVersionMS shr 16;
      Major2 := FileInfo.dwFileVersionMS and $FFFF;
      Minor1 := FileInfo.dwFileVersionLS shr 16;
      Minor2 := FileInfo.dwFileVersionLS and $FFFF;
    finally
      FreeMem(Info);
    end;
    Result := IntToStr(Major1) + '.' + IntToStr(Major2) + '.' + IntToStr(Minor1) + '.' + IntToStr(Minor2);
  except
    Result := '';
  end;
end;

function  IIF(Condition: boolean; TrueValue: string; FalseValue: string): string; overload;
begin
  if Condition then
    Result := TrueValue
  else
    Result := FalseValue;
end;

function IIF(Condition: boolean; TrueValue: TObject; FalseValue: TObject): TObject;
begin
  if Condition then
    Result := TrueValue
  else
    Result := FalseValue;
end;

function  IIF(Condition: boolean; TrueValue: double; FalseValue: double): double;
begin
  if Condition then
    Result := TrueValue
  else
    Result := FalseValue;
end;

function  IIF(Condition: boolean; TrueValue, FalseValue: integer): integer;
begin
  if Condition then
    Result := TrueValue
  else
    Result := FalseValue;
end;

procedure DisableProcessWindowsGhosting;
var
  DisableProcessWindowsGhostingImp : procedure;
begin
  @DisableProcessWindowsGhostingImp :=
    GetProcAddress(GetModuleHandle('user32.dll'),
    'DisableProcessWindowsGhosting');
  if (@DisableProcessWindowsGhostingImp <> nil) then
    DisableProcessWindowsGhostingImp;
end;

function  MonthName(Month: integer): string;
begin
  Result := MonthNames[Month];
end;

function GetTempPathWrapper: string;
var
  R: Cardinal;
begin
  Result := '';
  R := GetTempPath(0, nil);
  SetLength(Result, R);
  GetTempPath(R, PChar(Result));
  SetLength(Result, StrLen(PChar(Result)));
end;

function FileGetTempName(const Prefix: string): string;
var
  TempPath, TempFile: string;
  R: Cardinal;
begin
  TempPath := GetTempPathWrapper;
  SetLength(TempFile, MAX_PATH);
  R := GetTempFileName(PChar(TempPath), PChar(Prefix), 0, PChar(TempFile));
  if R <> 0 then
  begin
    SetLength(TempFile, StrLen(PChar(TempFile)));
    Result := TempFile;
  end;
end;

function  GetApplicationDataFolder: string;
var
  PDirectory: PChar;
begin
  SetLength(Result, MAX_PATH);
  PDirectory := PChar(Result);
  SHGetSpecialFolderPath(
    Application.Handle,
    PDirectory,
    CSIDL_APPDATA,
    true
  );
  // Обрезаем до терминирующего 0, добавляем завершающий \
  Result := IncludeTrailingPathDelimiter(PDirectory);
end;

function  InputString(const Prompt: string; var Value: string): boolean;
var
  F: TForm;
  E: TEdit;
begin
  F := TForm.Create(Application);
  with F do
    try
      Caption := 'Введите значение';
      with TLabel.Create(F) do
      begin
        Top := 8;
        Left := 8;
        Width := F.Width - 16;
        AutoSize := false;
        Anchors := Anchors + [akRight];
        Caption := Prompt;
        Parent := F;
      end;
      E := TEdit.Create(F);
      with E do
      begin
        Top := 24;
        Left := 8;
        Width := F.Width - 16;
        AutoSize := false;
        Anchors := Anchors + [akRight];
        Text := Value;
        Parent := F;
      end;
      ActiveControl := E;
      with TButton.Create(F) do
      begin
        Left := F.Width - Width * 2 - 16;
        Top := F.Height - Height - 8;
        Caption := 'OK';
        Default := true;
        ModalResult := mrOk;
        Anchors := [akRight, akBottom];
        Parent := F;
      end;
      with TButton.Create(F) do
      begin
        Left := F.Width - Width - 8;
        Top := F.Height - Height - 8;
        Caption := 'Отмена';
        Cancel := true;
        ModalResult := mrCancel;
        Anchors := [akRight, akBottom];
        Parent := F;
      end;
      Position := poScreenCenter;
      Height := 112;
      Result := ShowModal = mrOk;
      if Result then
        Value := E.Text;
    finally
      Free;
    end;
end;

function  IsNumber(const Value: string): boolean;
var
  i, n: integer;
  NumberChars: string;
begin
  Result := true;
  n := Length(Value);
  if n = 0 then
  begin
    Result := false;
    Exit;
  end;
  NumberChars := '0123456789' + FormatSettings.DecimalSeparator;
  for i := 1 to n do
  begin
    if (i = 1) and (Value[1] = '-') then
      Continue;
    if Pos(Value[i], NumberChars) = 0 then
    begin
      Result := false;
      Exit;
    end;
  end;
  i := Pos(FormatSettings.DecimalSeparator, Value);
  if (i > 0) and
     (Pos(FormatSettings.DecimalSeparator, Copy(Value, i, Length(Value) - i + 1)) > 0) then
    Result := false;
end;

function  IsInteger(const Value: string): boolean;
var
  i, n: integer;
  NumberChars: string;
begin
  Result := true;
  n := Length(Value);
  if n = 0 then
  begin
    Result := false;
    Exit;
  end;
  NumberChars := '0123456789';
  for i := 1 to n do
  begin
    if (i = 1) and (Value[1] = '-') then
      Continue;
    if Pos(Value[i], NumberChars) = 0 then
    begin
      Result := false;
      Exit;
    end;
  end;
end;

function GetComputerNetName: string;
var
  Buffer: string;
  Size: dword;
begin
  SetLength(Buffer, Size);
  size := 256;
  if GetComputerName(PChar(buffer), size) then
    Result := Buffer
  else
    Result := '';
end;

function VarArraysIsEqual(const A, B: variant): boolean;
var
  i: integer;
begin
  Result :=
    VarIsArray(A) and VarIsArray(B) and
    (VarArrayLowBound(A, 1) = VarArrayLowBound(B, 1)) and
    (VarArrayHighBound(A, 1) = VarArrayHighBound(B, 1));
  if not Result then
    Exit;
  for i := VarArrayLowBound(A, 1) to VarArrayHighBound(A, 1) do
    Result := Result and VarsIsEqual(A[i], B[i]);
end;

function VarsIsEqual(const A, B: variant): boolean;
begin
  Result :=
    (VarIsArray(A) and VarIsArray(B) and VarArraysIsEqual(A, B)) or
    ((not VarIsArray(B)) and (not VarIsArray(A)) and
     (VarCompareValue(A, B) = vrEqual));
end;

initialization
  LSettingsFileName := '';

end.
