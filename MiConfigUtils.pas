{
MiConfigUtils
===========
Por Tito Hinostroza 20/12/2016

Descripción
===========
Unidad con rutinas útiles para implementar las ventanas de configuración.
}
unit MiConfigUtils;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Graphics, Forms, ComCtrls;

type
  TlistFrames = array of TFrame;

  //Utilidades para el uso de Frames con "MiCOnfig"
  procedure HideAllFrames(form: TForm);
  procedure ShowFramePos(frm: TFrame; x, y: integer);
  //Utilidades para el uso de TTree con "MiCOnfig"
  function IdFromTTreeNode(node: TTreeNode): string;
  function TTreeNodeFromId(Id: string; tree: TTreeView): TTreeNode;

implementation

function ListOfFrames(form: TForm): TlistFrames;
//Devuelve la lista de frames del tipo TCfgFrame declarado aquí
var
  i: Integer;
  n : integer;
  f: TFrame;
begin
  SetLength(Result,0);
  for i:= 0 to form.ComponentCount-1 do begin
    if form.Components[i] is TFrame then begin
      f:=TFrame(form.Components[i]);  //obtiene referencia
      n := high(Result)+1;     //número de elementos
      setlength(Result, n+1);  //hace espacio
      Result[n] := f;          //agrega
    end;
  end;
end;
procedure HideAllFrames(form: TForm);
//Oculta todos los frames de un formulario
var
  f: TFrame;
begin
  for f in ListOfFrames(form) do
    f.visible := false;
end;
procedure ShowFramePos(frm: TFrame; x, y: integer);
//Muestra el frame en la posición indicada
begin
  frm.left:= x;
  frm.Top := y;
  frm.Visible:=true;
end;
function IdFromTTreeNode(node: TTreeNode): string;
//Returns an ID with indication of the position of a TTreeNode'.
//It has the form: 1, 1.1, 1.2. Only works for two levels.
var
  nivel: Integer;
begin
  nivel := node.Level;
  if nivel = 1 then  //de dos niveles
    Result := IntToStr(node.Parent.Index+1) + '.' +
             IntToStr(node.Index+1)
  else  //de un nivel
    Result := IntToStr(node.Index+1);
end;
function TTreeNodeFromId(Id: string; tree: TTreeView): TTreeNode;
//Returns a TreeNode, given the ID position. If not found, returns NIL.
//Only works for two levels.
var
  list: TStringList;
  it: TTreeNode;
  Padre: TTreeNode;
  i: Integer;
begin
  Result := nil;  //por defecto
  if Id='' then exit;
  list := TStringList.Create;
  list.Delimiter:='.';
  list.DelimitedText:=Id;
  if list.Count = 1 then begin  //de un solo nivel
    //ubica el nodo
    for it in Tree.Items do if it.Level=0 then begin
        if IntToStr(it.Index+1) = list[0] then Result := it;
    end;
  end else begin  //de dos o más niveles
    //ubica al nodo padre
    Padre := nil;
    for it in Tree.Items do begin
      if it.Level=0 then begin
        if IntToStr(it.Index+1) = list[0] then Padre := it;
      end;
    end;
    if Padre = nil then exit;  //no lo ubica
    //ubica al nodo hijo
    for i := 0 to Padre.Count-1 do begin
      it := Padre.Items[i];
      if it.Level=1 then begin
        if IntToStr(it.Index+1) = list[1] then Result := it;
      end;
    end;
  end;
  list.Destroy;
end;

initialization

finalization

end.

