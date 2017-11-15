unit FormConfig;
{$mode objfpc}{$H+}
interface
uses
  Forms, Graphics, Buttons, StdCtrls, MiConfigINI, MisUtils;
type
  { TConfig }
  TConfig = class(TForm)
    BitCancel: TBitBtn;
    BitAceptar: TBitBtn;
    Edit1: TEdit;
    Label2: TLabel;
    procedure BitAceptarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    //vars to manage
    MyText : string;
    procedure Initiate(f: TForm);
    procedure SaveToFile;
  end;

var
  Config: TConfig;

implementation
{$R *.lfm}

{ TConfig }
procedure TConfig.FormCreate(Sender: TObject);
begin
  cfgFile.VerifyFile;
end;

procedure TConfig.Initiate(f: TForm);
begin
  //asociate vars to controls
  cfgFile.Asoc_Str('MyText', @MyText, Edit1, '');
  if not cfgFile.FileToProperties then begin
    MsgErr(cfgFile.MsjErr);
  end;
end;

procedure TConfig.FormShow(Sender: TObject);
begin
  cfgFile.PropertiesToWindow;
end;

procedure TConfig.BitAceptarClick(Sender: TObject);
begin
  cfgFile.WindowToProperties;
  if cfgFile.MsjErr<>'' then begin
    MsgErr(cfgFile.MsjErr);
    exit;
  end;
  self.Close;
end;

procedure TConfig.SaveToFile;
begin
  if not cfgFile.PropertiesToFile then begin
    MsgErr(cfgFile.MsjErr);
  end;
end;

end.

