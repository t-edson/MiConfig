unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, FormConfig;

type

  { TForm1 }

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  Config.Initiate(self);   //necesario para poder trabajar
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  Config.Showmodal;
end;

end.

