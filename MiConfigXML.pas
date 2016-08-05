{
MiConfigXml 0.1b
=============
Por Tito Hinostroza 29/07/2016

Descripción
===========
Unidad con rutinas de lectura/escritura de propiedades en archivos XML. Permite crear
fácilmente, una ventana de configuración, con las opciones: ACEPTAR y CANCELAR.
Es similar a MiConfigINI, pero trabaja con archivos XML.

Para alamacenar las propiedades, se debe crear un objeto TMiConfigXML. Sin embargo,
la unidad crea por defecto, una instancia de TMiConfigXML, llamada "cfgFile", que toma
como nombre <nombre del proyecto>.xml
Tiene como dependencia a la librería MisUtils.

Por Tito Hinostroza 29/07/2016
}
unit MiConfigXML;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Graphics, Forms, Laz2_XMLCfg, MisUtils, MiConfigBasic;
type
  { TMiConfigXML }
  {Clase base que es usada para manejar los campos de configuración.}
  TMiConfigXML = class(TMiConfigBasic)
  private
    fileName    : string;   //archivo XML
    function DefaultFileName: string;
    procedure FileProperty(xmlCfg: TXMLConfig; const r: TParElem; FileToProp: boolean);
  public
    secINI: string;   //sección donde se guardarán los datos en un archivo INI
    procedure VerifyFile;
    function FileToProperties: boolean; virtual;
    function PropertiesToFile: boolean; virtual;
  public  //Constructor y Destructor
    constructor Create(XMLfile0: string);
    destructor Destroy; override;
  end;

var
  cfgFile: TMiConfigXML;   //Default XML Config file

implementation



{ TMiConfigXML }
function TMiConfigXML.DefaultFileName: string;
{Devuelve el nombre pro defecto del archvio de configuración}
begin
  Result := ChangeFileExt(Application.ExeName,'.xml');
end;
procedure TMiConfigXML.VerifyFile;
//Verifica si el archivo XML "FileName" existe. Si no, muestra un mensaje y lo crea.
var
  F: textfile;
begin
  if not FileExists(fileName) then begin
    MsgErr('No XML file found: %s', [fileName]);
    //crea uno vacío para leer las opciones por defecto
    AssignFile(F, fileName);
    Rewrite(F);
    writeln(F, '<?xml version="1.0" encoding="utf-8"?>');
    writeln(F, '<CONFIG>');
    writeln(F, '</CONFIG>');
    CloseFile(F);
  end;
end;
procedure TMiConfigXML.FileProperty(xmlCfg: TXMLConfig; const r: TParElem; FileToProp: boolean);
{Permite leer o escribir una propiedad en el archivo XML}
var
  s, defStr: String;
  c: TColor;
  list: TStringList;
begin
  if r.pVar = nil then exit;   //se inició con NIL
  case r.tipPar of
  tp_Int, tp_Int_TEdit, tp_Int_TSpinEdit:
    if FileToProp then begin  //lee entero
      r.AsInteger := xmlCfg.GetValue(r.etiqVar + '/Val', r.defInt)
    end else begin
      xmlCfg.SetValue(r.etiqVar + '/Val', r.AsInteger) ;
    end;
  //---------------------------------------------------------------------
  tp_Dbl, tp_Dbl_TEdit, tp_Dbl_TFloatSpinEdit:
    //No hay métodos para leer/escribir números flotantes. Se usarán cadena
    if FileToProp then begin
      defStr := FloatToStr(r.defDbl);
      s := xmlCfg.GetValue(r.etiqVar + '/Val', defStr);  //lee como cadena
      r.AsDouble := StrToFloat(s);
    end else begin
      s := FloatToStr(r.AsDouble);
      xmlCfg.SetValue(r.etiqVar + '/Val', s) ;
    end;
  //---------------------------------------------------------------------
  tp_Str, tp_Str_TEdit, tp_Str_TEditButton, tp_Str_TCmbBox:
    if FileToProp then begin  //lee cadena
      r.AsString := xmlCfg.GetValue(r.etiqVar + '/Val', r.defStr);
    end else begin
      xmlCfg.SetValue(r.etiqVar + '/Val', r.AsString) ;
    end;
  //---------------------------------------------------------------------
  tp_Bol, tp_Bol_TCheckBox, tp_Bol_TRadBut:
    if FileToProp then begin  //lee booleano
      r.AsBoolean := xmlCfg.GetValue(r.etiqVar + '/Val', r.defBol);
    end else begin
      xmlCfg.SetValue(r.etiqVar + '/Val', r.AsBoolean);
    end;
  //---------------------------------------------------------------------
  tp_Enum, tp_Enum_TRadBut, tp_Enum_TRadGroup:
    if FileToProp then begin  //lee enumerado como entero
       if r.lVar = 4 then begin  //tamaño común de las variable enumeradas
         r.AsInt32 := xmlCfg.GetValue(r.etiqVar + '/Val', r.defInt);
       end else begin  //tamaño no implementado
         msjErr := dic('Enumerated type no handled.');
         exit;
       end;
    end else begin
      if r.lVar = 4 then begin
        xmlCfg.SetValue(r.etiqVar + '/Val', r.AsInt32) ;
      end else begin  //tamaño no implementado
        msjErr := dic('Enumerated type no handled.');
        exit;
      end;
    end;
  //---------------------------------------------------------------------
  tp_TCol_TColBut:
    if FileToProp then begin  //lee TColor
      r.AsTColor := xmlCfg.GetValue(r.etiqVar + '/Val',  r.defCol);  //lee como entero
    end else begin
      c := r.AsTColor;
      xmlCfg.SetValue(r.etiqVar + '/Val', c) ;  //escribe como entero
    end;
  tp_StrList, tp_StrList_TListBox:
    if FileToProp then  begin //lee TStringList
      list := TStringList(r.Pvar^);
      list.Text := xmlCfg.GetValue(r.etiqVar + '/Val', '');  //lee como texto
    end else begin
      list := TStringList(r.Pvar^);
      xmlCfg.SetValue(r.etiqVar + '/Val', list.Text);  //escribe como texto
    end;
  else  //no se ha implementado bien
    msjErr := dic('Design error.');
    exit;
  end;
end;
function TMiConfigXML.FileToProperties: boolean;
{Lee de disco las propiedades registradas
Si encuentra error devuelve FALSE, y el mensaje de error en "MsjErr".}
var
  r: TParElem;
  xmlCfg: TXMLConfig;
begin
  if not FileExists(fileName) then begin
    MsjErr := dic('XML file does not exist.');  //errro
    exit(false);  //para que no intente leer
  end;
  //Asume error por defecto
  Result := false;
  MsjErr := dic('Error reading INI file: %s', [fileName]);
  try
    xmlCfg := TXMLConfig.Create(nil);
    xmlCfg.Filename := fileName;  //lee archivo XML
    for r in listParElem do begin
      FileProperty(xmlCfg, r, true);
      if r.OnFileToProperty<>nil then r.OnFileToProperty;
    end;
    Result := true;  //sin error
    MsjErr := '';    //sin error
  finally
    xmlCfg.Destroy;
  end;
end;
function TMiConfigXML.PropertiesToFile: boolean;
{Guarda en disco las propiedades registradas
Si encuentra error devuelve FALSE, y el mensaje de error en "MsjErr".}
var
  r: TParElem;
  xmlCfg: TXMLConfig;
begin
  Result := false;
  MsjErr := dic('Error writing INI file: %s', [fileName]);
  try
    If FileExists(fileName)  Then  begin  //ve si existe
       If FileIsReadOnly(fileName) Then begin
          MsjErr := dic('INI file is only read.');
          exit(false);
       End;
    End;
    xmlCfg := TXMLConfig.Create(nil);
    xmlCfg.Filename := fileName;  //lee archivo XML
    xmlCfg.Clear;
    for r in listParElem do begin
      if r.OnPropertyToFile<>nil then r.OnPropertyToFile;  //se ejecuta antes
      FileProperty(xmlCfg, r, false);
    end;
    xmlCfg.Flush;
    Result := true;  //sin error
    MsjErr := '';    //sin error
  finally
    xmlCfg.Destroy;
  end;
end;
//Constructor y Destructor
constructor TMiConfigXML.Create(XMLfile0: string);
begin
  inherited Create;
  fileName := XMLfile0;
end;
destructor TMiConfigXML.Destroy;
begin
  inherited Destroy;
end;

initialization
  cfgFile := TMiConfigXML.Create(cfgFile.DefaultFileName);

finalization
  cfgFile.Destroy;
end.

