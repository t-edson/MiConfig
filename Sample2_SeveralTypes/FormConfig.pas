{Modelo de formulario de configuración que usa solo un Frame de configuración}
unit FormConfig;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, Spin, MisUtils,
  MiConfigXML;  //Change to MiConfigINI, to use INI file instead.
type
  TMyEnum = (First, Second, Third);

  { TConfig }
  TConfig = class(TForm)
    BitCancel: TBitBtn;
    BitAceptar: TBitBtn;
    CheckBox1: TCheckBox;
    ColorButton1: TColorButton;
    Edit1: TEdit;
    Edit2: TEdit;
    FloatSpinEdit1: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Memo1: TMemo;
    RadButton1: TRadioButton;
    RadButton2: TRadioButton;
    RadButton3: TRadioButton;
    procedure BitAceptarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    msjError: string;    //Error string

    //vars to manage
    MyString : string;
    MyInt  : integer;
    MyFloat: Double;
    MyBool : Boolean;
    MyColor: TColor;
    MyEnum : TMyEnum;
    MyMultStr: string;
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
  cfgFile.Asoc_Str('MyText', @MyString, Edit1, '');
  cfgFile.Asoc_Int('MyInt', @MyInt, Edit2, 5, 1,7);
  cfgFile.Asoc_Dbl('MyFloat', @MyFloat, FloatSpinEdit1, 3.14);
  cfgFile.Asoc_Bol('MyBool', @MyBool, CheckBox1, false);
  cfgFile.Asoc_TCol('MyColor', @MyColor, ColorButton1, clWhite);
  cfgFile.Asoc_Enum('MyEnum', @MyEnum, SizeOf(TMyEnum),
                    [RadButton1, RadButton2, RadButton3],0);
  cfgFile.Asoc_Str('MyMultStr', @MyMultStr, Memo1, 'line1' + LineEnding + 'line2');
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

