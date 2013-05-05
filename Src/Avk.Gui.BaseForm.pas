unit Avk.GUI.BaseForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Contnrs;

type
  TBaseForm = class (TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FAutoSaveFormSettings: boolean;
    procedure SetAutoSaveFormSettings(const Value: boolean);
  public
    { Public declarations }
    FreeOnClose: boolean;

    function  KeepBoundsRec: boolean; virtual;
    function  FormSectionName: string; virtual;
    procedure SaveFormSettingsValue(Name: string; Value: string); overload;
    function  LoadFormSettingsValue(Name: string): string; overload;
    procedure SaveFormSettingsValue(Name: string; Value: boolean); overload;
    function  LoadFormSettingsValue(Name: string; DefaultValue: boolean): boolean; overload;
    procedure LoadFormSettings; virtual;
    procedure SaveFormSettings; virtual;
    property  AutoSaveFormSettings: boolean read FAutoSaveFormSettings write SetAutoSaveFormSettings;

    class function CreateAndShowModal: TModalResult;
    class function CreateAndShow: TBaseForm;
  end;

implementation

uses
  TabOrder,
  uUtils;

const
  FormBoundsSettingsName = 'Bounds';

{$R *.dfm}

class function TBaseForm.CreateAndShow: TBaseForm;
begin
  Result := Self.Create(Application);
  Result.Show;
end;

procedure TBaseForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FreeOnClose then
    Action := caFree;
end;

procedure TBaseForm.FormCreate(Sender: TObject);
begin
  FreeOnClose := true;
  AutoSaveFormSettings := true;
  LoadFormSettings;
end;

procedure TBaseForm.FormDestroy(Sender: TObject);
begin
  if AutoSaveFormSettings then
    SaveFormSettings;
end;

procedure TBaseForm.LoadFormSettings;
var
  Str: string;
begin
  if KeepBoundsRec then
  begin
    Str := LoadFormSettingsValue(FormBoundsSettingsName);
    if Str <> '' then
      BoundsRect := StringToRect(Str);
  end;
end;

procedure TBaseForm.SaveFormSettings;
begin
  if KeepBoundsRec then
  begin
    SaveFormSettingsValue(
      FormBoundsSettingsName,
      RectToString(BoundsRect)
    );
  end;
end;

procedure TBaseForm.SetAutoSaveFormSettings(const Value: boolean);
begin
  FAutoSaveFormSettings := Value;
end;

procedure TBaseForm.FormShow(Sender: TObject);
begin
  UpdateTabOrders(Self);
end;

function TBaseForm.LoadFormSettingsValue(Name: string): string;
begin
  Result := LoadSettingsValue(FormSectionName, Name);
end;

procedure TBaseForm.SaveFormSettingsValue(Name, Value: string);
begin
  SaveSettingsValue(FormSectionName, Name, Value);
end;

function TBaseForm.FormSectionName: string;
begin
  Result := Self.ClassName;
end;

class function TBaseForm.CreateAndShowModal: TModalResult;
begin
  with Self.Create(Application) do
    try
      Result := ShowModal;
    finally
      Free;
    end;
end;

function TBaseForm.LoadFormSettingsValue(Name: string; DefaultValue: boolean): boolean;
begin
  try
    Result := StrToBool(LoadFormSettingsValue(Name));
  except
    Result := DefaultValue;
  end;
end;

procedure TBaseForm.SaveFormSettingsValue(Name: string; Value: boolean);
begin
  SaveFormSettingsValue(Name, BoolToStr(Value));
end;

function TBaseForm.KeepBoundsRec: boolean;
begin
  Result := true;
end;

end.
