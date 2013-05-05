unit Avk.Core.Exceptions;

interface

uses
  Classes, SysUtils;

type
  EInternalError = class (Exception)
  private
    FAdditionalInfo: TStrings;
    FIncludeStackList: boolean;

    procedure AddStackListToInfo;
  public
    destructor Destroy; override;
    // У Exception слишком много конструкторов
    //  неохота переопределять каждый ;-)
    procedure AfterConstruction; override;

    procedure FillAdditionalInfo; virtual;

    property IncludeStackList: boolean
      read FIncludeStackList write FIncludeStackList;
    property AdditionalInfo: TStrings read FAdditionalInfo;
  end;

  EUserError = class (EInternalError)
  public
    procedure AfterConstruction; override;
  end;

type
  TExceptionHandlerProc = procedure (Sender: TObject; E: Exception; var AHandled: boolean) of object;

procedure RaiseUserError(Message: string);
procedure RaiseUserErrorFmt(Format: string; const Args: array of const);
procedure RaiseInternalError(Message: string);
procedure RaiseInternalErrorFmt(Format: string; const Args: array of const);

var
  ExceptionHandler: TExceptionHandlerProc;

implementation

uses
  Forms, Variants,
  JclDebug;

procedure RaiseUserError(Message: string);
begin
  raise EUserError.Create(Message);
end;

procedure RaiseUserErrorFmt(Format: string; const Args: array of const);
begin
  raise EUserError.CreateFmt(Format, Args);
end;

procedure RaiseInternalError(Message: string);
begin
  raise EInternalError.Create(Message);
end;

procedure RaiseInternalErrorFmt(Format: string; const Args: array of const);
begin
  raise EInternalError.CreateFmt(Format, Args);
end;

{ EInternalError }

destructor EInternalError.Destroy;
begin
  FAdditionalInfo.Free;
  inherited;
end;

procedure EInternalError.FillAdditionalInfo;
begin
  FAdditionalInfo.Clear;
  with FAdditionalInfo do
  begin
    Add('АРМ: ' + ExtractFileName(Application.ExeName));
    Add('Дата: ' + DateToStr(Date));
    Add('Время: ' + TimeToStr(Time));
    Add('Текст ошибки: ' + Message);
    if IncludeStackList then
      AddStackListToInfo;
  end;
end;

procedure EInternalError.AddStackListToInfo;
var
  i: integer;
  StackList: TJclStackInfoList;
  LocationInfo: TJclLocationInfo;
begin
  FAdditionalInfo.Add('');
  FAdditionalInfo.Add('Стек вызовов:');
  StackList := JclLastExceptStackList;
  for i := 0 to StackList.Count - 1 do
  begin
    LocationInfo := GetLocationInfo(
      Pointer(StackList[i].StackInfo.CallerAddr));
    FAdditionalInfo.Add(
      LocationInfo.UnitName + '; ' +
      LocationInfo.ProcedureName + '; ' +
      IntToStr(LocationInfo.LineNumber));
  end;
end;

procedure EInternalError.AfterConstruction;
begin
  inherited;
  FIncludeStackList := true;
  FAdditionalInfo := TStringList.Create;
end;

{ EUserError }

procedure EUserError.AfterConstruction;
begin
  inherited;
  IncludeStackList := false;
end;

initialization
  Include(JclStackTrackingOptions, stRawMode);
  JclStartExceptionTracking;

finalization
  JclStopExceptionTracking;

end.
