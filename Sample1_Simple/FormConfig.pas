unit FormConfig;
{$mode objfpc}{$H+}
interface
uses
  Forms, Graphics, Buttons, StdCtrls, MiConfigINI;
type
  { TConfig }
  TConfig = class(TForm)
    BitCancel: TBitBtn;
    BitOK: TBitBtn;
    Edit1: TEdit;
    procedure BitOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    //Variables to manage
    MyText : string;
    procedure Initiate(f: TForm);
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
    Application.MessageBox(PChar(cfgFile.MsjErr), '');
  end;
end;

procedure TConfig.FormShow(Sender: TObject);
begin
  cfgFile.PropertiesToWindow;
end;

procedure TConfig.BitOKClick(Sender: TObject);
begin
  cfgFile.WindowToProperties;
  if cfgFile.MsjErr<>'' then begin
    Application.MessageBox(PChar(cfgFile.MsjErr), '');
    exit;
  end;
  self.Close;
end;

procedure TConfig.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if not cfgFile.PropertiesToFile then begin
    Application.MessageBox(PChar(cfgFile.MsjErr), '');
  end;
end;

end.

