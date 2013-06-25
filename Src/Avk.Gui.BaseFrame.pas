unit Avk.Gui.BaseFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, dxSkinsCore,
  cxScrollBox, cxLabel, Vcl.ExtCtrls, System.Contnrs, uFormErrors,
  dxSkinsDefaultPainters;

type
  TBaseFrame = class (TFrame)
    TopLabel: TcxLabel;
    ClientPanel: TPanel;
    FormErrors: TFormErrors;
  private
    FCreatedObjects: TObjectList;
    { Private declarations }
  public
    { Public declarations }
    AutoSaveFormSettings: boolean;

    constructor Create(AOwner: TCOmponent); override;
    destructor Destroy; override;

    function FrameSectionName: string; virtual;
    procedure SaveFrameSettingsValue(Name: string; Value: string); overload;
    function  LoadFrameSettingsValue(Name: string): string; overload;
    procedure SaveFrameSettingsValue(Name: string; Value: boolean); overload;
    function  LoadFrameSettingsValue(Name: string; DefaultValue: boolean): boolean; overload;

    procedure SaveFrameSettings; virtual;
    procedure LoadFrameSettings; virtual;

    property  CreatedObjects: TObjectList read FCreatedObjects;
  end;

implementation

uses
  AVK.Core.Utils;

{$R *.dfm}

{ TBaseFrame }

constructor TBaseFrame.Create(AOwner: TCOmponent);
begin
  inherited;
  AutoSaveFormSettings := true;
  FCreatedObjects := TObjectList.Create;
end;

destructor TBaseFrame.Destroy;
begin
  FCreatedObjects.Free;
  inherited;
end;

function TBaseFrame.FrameSectionName: string;
begin
  Result := ClassName;
end;

function TBaseFrame.LoadFrameSettingsValue(Name: string): string;
begin
  Result := LoadSettingsValue(FrameSectionName, Name);
end;

procedure TBaseFrame.SaveFrameSettingsValue(Name, Value: string);
begin
  SaveSettingsValue(FrameSectionName, Name, Value);
end;

procedure TBaseFrame.LoadFrameSettings;
begin
  ;
end;

function TBaseFrame.LoadFrameSettingsValue(Name: string; DefaultValue: boolean): boolean;
var
  V: string;
begin
  V := LoadFrameSettingsValue(Name);
  if V = '' then
    Result := DefaultValue
  else
    try
      Result := StrToBool(LoadFrameSettingsValue(Name));
    except
      Result := DefaultValue;
    end;
end;

procedure TBaseFrame.SaveFrameSettings;
begin

end;

procedure TBaseFrame.SaveFrameSettingsValue(Name: string; Value: boolean);
begin
  SaveFrameSettingsValue(Name, BoolToStr(Value));
end;

end.
