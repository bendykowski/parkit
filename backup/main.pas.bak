unit main;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF Windows}Windows,{$ELSE}LCLIntf, LCLType,{$ENDIF} LCLProc, LazHelpHTML,
  UTF8Process, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Menus, Buttons, Clipbrd, Spin, ColorUtils, IniFiles,
  ShlObj, AppLang, Process;

type

  { TFMain }

  ColBuffer = record
    Color: TRGBColor;
    History: Array[0..4] of TRGBColor;
    Undos: Byte;
    Used: Boolean;
  end;

  TFMain = class(TForm)
    ApplicationProperties: TApplicationProperties;
    BCopy: TButton;
    BPick: TButton;
    BRuler: TButton;
    BSet: TButton;
    CBColorFormat: TComboBox;
    CBHideToTray: TCheckBox;
    CBShowTray: TCheckBox;
    CBOnTop: TCheckBox;
    CBUseSafeWebColors: TCheckBox;
    CBUseCapitalLetters: TCheckBox;
    ColorDialog: TColorDialog;
    CBLanguage: TComboBox;
    CompColorDialog: TColorDialog;
    EPayPalAccount: TEdit;
    EHistCode0: TEdit;
    EHistCode1: TEdit;
    EHistCode2: TEdit;
    EHistCode3: TEdit;
    EHistCode4: TEdit;
    EBankAccount: TEdit;
    GBButtons: TGroupBox;
    GBCoordinates: TGroupBox;
    GBMeasurement: TGroupBox;
    IAppLogo: TImage;
    IHSVValue: TImage;
    IHSVSaturation: TImage;
    IRGBRefresh: TImage;
    IHSVRefresh: TImage;
    ISafeColor: TImage;
    IRGBRed: TImage;
    IRGBGreen: TImage;
    IRGBBlue: TImage;
    IHSVHue: TImage;
    LHSVColorNew: TLabel;
    LRGBColorOryginal: TLabel;
    LBankCaption: TLabel;
    LPayPalCaption: TLabel;
    LRGBColorNew: TLabel;
    LHSVColorOryginal: TLabel;
    LSupportDescription: TLabel;
    LDelayMin: TLabel;
    LAlphaValue: TLabel;
    LDelayMax: TLabel;
    LLanguageCaption: TLabel;
    LTransparency: TLabel;
    LColorCurrent: TLabel;
    LRGBColor: TLabel;
    LHSVColor: TLabel;
    LHSVSetValue: TLabel;
    LHSVSetSaturation: TLabel;
    LSetRed: TLabel;
    LColorHistory: TLabel;
    LAppAuthor: TLabel;
    LAppGreetings: TLabel;
    LAppMail: TLabel;
    LAppMailCaption: TLabel;
    LAppName: TLabel;
    LAppWww: TLabel;
    LAppWwwCaption: TLabel;
    LBPoint: TLabel;
    LBPointCaption: TLabel;
    LDiagonal: TLabel;
    LDiagonalCaption: TLabel;
    LEPoint: TLabel;
    LEPointCaption: TLabel;
    LHeight: TLabel;
    LHeightCaption: TLabel;
    LSetGreen: TLabel;
    LSetBlue: TLabel;
    LHSVSetHue: TLabel;
    LWidth: TLabel;
    LWidthCaption: TLabel;
    MainMenu: TMainMenu;
    MAppDescription: TMemo;
    MMILine2: TMenuItem;
    MMISupport: TMenuItem;
    MMIClose: TMenuItem;
    MMILine: TMenuItem;
    MMISettings: TMenuItem;
    MMIInfo: TMenuItem;
    MMIPicker: TMenuItem;
    MMIOther: TMenuItem;
    MMIRuler: TMenuItem;
    MIClose: TMenuItem;
    MItemBreak: TMenuItem;
    MILinijka: TMenuItem;
    MIPicker: TMenuItem;
    NInfo: TNotebook;
    NPickerTools: TNotebook;
    NPages: TNotebook;
    PHSVColorOriginal: TPanel;
    PRGBColorOriginal: TPanel;
    PSupport: TPage;
    PDescription: TPage;
    PSettings: TPage;
    PColorHSV: TPage;
    PRGBColor: TPanel;
    PColorRGB: TPage;
    PHistColor0: TPanel;
    PColorHistory: TPage;
    PCol11: TPanel;
    PColorView: TPage;
    PCurrentColor: TPanel;
    PHistColor1: TPanel;
    PHistColor2: TPanel;
    PHistColor3: TPanel;
    PHistColor4: TPanel;
    PHSVColor: TPanel;
    PRuler: TPage;
    PPicker: TPage;
    PInfo: TPage;
    PBuffers: TPanel;
    PCol0: TPanel;
    PCol1: TPanel;
    PCol10: TPanel;
    PCol2: TPanel;
    PCol3: TPanel;
    PCol4: TPanel;
    PCol5: TPanel;
    PCol6: TPanel;
    PCol7: TPanel;
    PCol8: TPanel;
    PCol9: TPanel;
    SBHSVAbandon: TSpeedButton;
    SBHSVConfirm: TSpeedButton;
    SBHSV: TSpeedButton;
    SBUndo: TSpeedButton;
    SBHistReturn: TSpeedButton;
    SBRGB: TSpeedButton;
    SEHSVSetValue: TSpinEdit;
    SEHSVSetSaturation: TSpinEdit;
    SESetRed: TSpinEdit;
    SESetGreen: TSpinEdit;
    SESetBlue: TSpinEdit;
    SBRGBConfirm: TSpeedButton;
    SBRGBAbandon: TSpeedButton;
    SEHSVSetHue: TSpinEdit;
    TBHSVSetValue: TTrackBar;
    TBSetGreen: TTrackBar;
    TBSetBlue: TTrackBar;
    TBHSVSetSaturation: TTrackBar;
    TBHSVSetHue: TTrackBar;
    TColor: TEdit;
    TBSetRed: TTrackBar;
    TBTransparency: TTrackBar;
    TrayPopupMenu: TPopupMenu;
    TBPickDelay: TTrackBar;
    TrayIcon: TTrayIcon;
    procedure ApplicationPropertiesMinimize(Sender: TObject);
    procedure BCopyClick(Sender: TObject);
    procedure BRulerClick(Sender: TObject);
    procedure BPickClick(Sender: TObject);
    procedure BSetClick(Sender: TObject);
    procedure CBColorFormatChange(Sender: TObject);
    procedure CBLanguageChange(Sender: TObject);
    procedure CBOnTopChange(Sender: TObject);
    procedure CBShowTrayChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure IHSVRefreshClick(Sender: TObject);
    procedure IRGBRefreshClick(Sender: TObject);
    procedure ISafeColorClick(Sender: TObject);
    procedure LAppWwwClick(Sender: TObject);
    procedure LAppWwwMouseEnter(Sender: TObject);
    procedure LAppWwwMouseLeave(Sender: TObject);
    procedure LAppWwwMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MICloseClick(Sender: TObject);
    procedure MMICloseClick(Sender: TObject);
    procedure MMISettingsClick(Sender: TObject);
    procedure MMIInfoClick(Sender: TObject);
    procedure MMIRulerClick(Sender: TObject);
    procedure MMIPickerClick(Sender: TObject);
    procedure MMISupportClick(Sender: TObject);
    procedure PCol0Click(Sender: TObject);
    procedure PHSVColorClick(Sender: TObject);
    procedure PRGBColorClick(Sender: TObject);
    procedure PCurrentColorClick(Sender: TObject);
    procedure PHistColor0Click(Sender: TObject);
    procedure SBHSVAbandonClick(Sender: TObject);
    procedure SBHSVClick(Sender: TObject);
    procedure SBHSVConfirmClick(Sender: TObject);
    procedure SBRGBAbandonClick(Sender: TObject);
    procedure SBRGBClick(Sender: TObject);
    procedure SBRGBConfirmClick(Sender: TObject);
    procedure SBUndoClick(Sender: TObject);
    procedure SBHistReturnClick(Sender: TObject);
    procedure SEHSVSetHueChange(Sender: TObject);
    procedure SESetRedChange(Sender: TObject);
    procedure TBHSVSetHueChange(Sender: TObject);
    procedure TBHSVSetSaturationChange(Sender: TObject);
    procedure TBHSVSetValueChange(Sender: TObject);
    procedure TBSetBlueChange(Sender: TObject);
    procedure TBSetGreenChange(Sender: TObject);
    procedure TBSetRedChange(Sender: TObject);
    procedure TBTransparencyChange(Sender: TObject);
    procedure TColorKeyPress(Sender: TObject; var Key: char);
    procedure TrayIconClick(Sender: TObject);

    procedure NewCurrentColor(rgbColor: TRGBColor);
    procedure SetCurrentColor(rgbColor: TRGBColor);
    procedure PaintView();
    procedure SetRGBColor(rgbColor: TRGBColor);
    procedure SetHSVColor(hsvColor: THSVColor);
    procedure PaintRGBView();
    procedure PaintHSVView();

    procedure AddColorToHistory(rgbColor: TRGBColor);

    function CaptureDesktop(): TBitmap;

    function GetAppDataDirectory(): String;
  private
  public
    currentColor: TRGBColor;

    supRGBColor: TRGBColor;
    supHSVColor: THSVColor;

    colorsArray: Array[0..11] of ColBuffer;
    colPointer: Byte;

    iniPath: String;
  end;

var
  FMain: TFMain;

implementation

uses
  desktop;

{$R *.lfm}

{

  ++++++++++++++++++++ My functions ++++++++++++++++++++

}
function TFMain.CaptureDesktop(): TBitmap;
var
  ScreenDC: HDC;
begin
  // Get handler of Desktop
  ScreenDC := GetDC(0);

  Result.LoadFromDevice(ScreenDC);

  ReleaseDC(0, ScreenDC);
  DeleteDC(ScreenDC);
  { do schowka }
  //Clipboard.Assign(bitmap);
end;

procedure TFMain.NewCurrentColor(rgbColor: TRGBColor);
begin
  AddColorToHistory(currentColor);

  SetCurrentColor(rgbColor);
end;

procedure TFMain.SetCurrentColor(rgbColor: TRGBColor);
begin
  if (CBUseSafeWebColors.Checked = false) then
    currentColor := rgbColor
  else
    currentColor := SafeWebColor(rgbColor);

  colorsArray[colPointer].Color := currentColor;

  SetRGBColor(currentColor);
  SetHSVColor(RGBColorToHSVColor(currentColor));

  PaintView();
end;

procedure TFMain.AddColorToHistory(rgbColor: TRGBColor);
var
  I: Byte;
begin
  if (colorsArray[colPointer].Used = false) then begin
    colorsArray[colPointer].Used := true;
  end else begin
    if (not EqualColors(rgbColor, colorsArray[colPointer].History[0])) then begin
      if (colorsArray[colPointer].Undos <= High(colorsArray[colPointer].History)) then
        Inc(colorsArray[colPointer].Undos);

      if (colorsArray[colPointer].Undos > 0) then begin
        for I := colorsArray[colPointer].Undos - 1 downto 0 do
          colorsArray[colPointer].History[I] := colorsArray[colPointer].History[I - 1];
      end;
    end;
  end;

  colorsArray[colPointer].History[0] := rgbColor;
end;

procedure TFMain.SetRGBColor(rgbColor: TRGBColor);
begin
  if (CBUseSafeWebColors.Checked = false) then
    supRGBColor := rgbColor
  else
    supRGBColor := SafeWebColor(rgbColor);

  PaintRGBView();
end;

procedure TFMain.SetHSVColor(hsvColor: THSVColor);
begin
  if (CBUseSafeWebColors.Checked = false) then
    supHSVColor := hsvColor
  else
    supHSVColor := RGBColorToHSVColor(SafeWebColor(HSVColorToRGBColor(hsvColor)));

  PaintHSVView();
end;

procedure TFMain.PaintView();
var
  I: Byte;
  compBuffer, compHistPanel, compHistText: TComponent;
begin
  if (colorsArray[colPointer].Used = false) then
    currentColor := RGBToRGBColor(255, 255, 255);

  PCurrentColor.Color := RGBColorToColor(currentColor);
  TColor.Text := ColorToStr(currentColor, CBColorFormat.Text, CBUseCapitalLetters.Checked);

  try
    for I := 0 to High(colorsArray) do
    begin
      compBuffer := FindComponent('PCol' + IntToStr(I));
      if compBuffer is TPanel then
      begin
        if (I = colPointer) then begin
          TPanel(compBuffer).BevelInner := bvLowered;
          TPanel(compBuffer).BevelOuter := bvLowered;
        end
        else begin
          TPanel(compBuffer).BevelInner := bvNone;
          TPanel(compBuffer).BevelOuter := bvNone;
        end;
        if (colorsArray[I].Used = true) then
          TPanel(compBuffer).Color := RGBColorToColor(colorsArray[I].Color)
        else
          TPanel(compBuffer).Color := clWhite;
      end;
    end;
  finally
  end;

  try
    for I := 0 to High(colorsArray[colPointer].History) do begin
      compHistPanel := FindComponent('PHistColor' + IntToStr(I));
      if compHistPanel is TPanel then begin
        if (I < colorsArray[colPointer].Undos) then begin
          TPanel(compHistPanel).Color := RGBColorToColor(colorsArray[colPointer].History[I]);
          TPanel(compHistPanel).Cursor := crHandPoint;
        end
        else begin
          TPanel(compHistPanel).Color  := clBtnFace;
          TPanel(compHistPanel).Cursor := crDefault;
        end;
      end;

      compHistText := FindComponent('EHistCode' + IntToStr(I));
      if compHistText is TEdit then begin
        if (I < colorsArray[colPointer].Undos) then
          TEdit(compHistText).Text := ColorToStr(colorsArray[colPointer].History[I], CBColorFormat.Text, CBUseCapitalLetters.Checked)
        else
          TEdit(compHistText).Text := '';
      end;
    end;
  finally
  end;

  if (colorsArray[colPointer].Undos > 0) then
    SBUndo.Enabled := true
  else
    SBUndo.Enabled := false;

  // if there isn't history in this buffer
  if ((NPickerTools.PageIndex = 1) AND (colorsArray[colPointer].Undos <= 0)) then
    NPickerTools.PageIndex := 0;

  // safe web color
  if (IsSafeWebColor(TColorToRGBColor(PCurrentColor.Color)) = True) then
    ISafeColor.Visible := False
  else
    ISafeColor.Visible := True;

  PaintRGBView();
  PaintHSVView();
end;

procedure TFMain.PaintRGBView();
var
  I, colStep: Word;
  colTmp: TColorRef;
  changeEvent: TNotifyEvent;
begin
  PRGBColor.Color := RGBColorToColor(supRGBColor);

  {
    Block OnChange Events until set them all
  }
  changeEvent := SESetRed.OnChange;
  SESetRed.OnChange := nil;
  SESetGreen.OnChange := nil;
  SESetBlue.OnChange := nil;

  TBSetRed.Position := supRGBColor.Red;
  TBSetGreen.Position := supRGBColor.Green;
  TBSetBlue.Position := supRGBColor.Blue;

  SESetRed.Value := supRGBColor.Red;
  SESetGreen.Value := supRGBColor.Green;
  SESetBlue.Value := supRGBColor.Blue;

  {
    Unblock onChange Events
  }
  SESetRed.OnChange := changeEvent;
  SESetGreen.OnChange := changeEvent;
  SESetBlue.OnChange := changeEvent;

  {
    Red TrackBar Image
  }
  with IRGBRed.Canvas do
  begin
    Pen.Width := 1;
    Pen.Style := psSolid;
  end;
  colStep := Trunc(SESetRed.MaxValue / IRGBRed.Width);
  for I := 0 to IRGBRed.Width do begin
    colTmp := RGB(I * colStep, SESetGreen.Value, SESetBlue.Value);
    with IRGBRed.Canvas do
    begin
      Pen.Color := colTmp;
      MoveTo(I, 0);
      LineTo(I, IRGBRed.Height);
    end;
  end;

  {
    Green TrackBar Image
  }
  with IRGBGreen.Canvas do
  begin
    Pen.Width := 1;
    Pen.Style := psSolid;
  end;
  colStep := Trunc(SESetGreen.MaxValue / IRGBGreen.Width);
  for I := 0 to IRGBGreen.Width do begin
    colTmp := RGB(SESetRed.Value, I * colStep, SESetBlue.Value);
    with IRGBGreen.Canvas do
    begin
      Pen.Color := colTmp;
      MoveTo(I, 0);
      LineTo(I, IRGBGreen.Height);
    end;
  end;

  with IRGBBlue.Canvas do
  begin
    Pen.Width := 1;
    Pen.Style := psSolid;
  end;
  colStep := Trunc(SESetBlue.MaxValue / IRGBBlue.Width);
  for I := 0 to IRGBBlue.Width do begin
    colTmp := RGB(SESetRed.Value, SESetGreen.Value, I * colStep);
    with IRGBBlue.Canvas do
    begin
      Pen.Color := colTmp;
      MoveTo(I, 0);
      LineTo(I, IRGBBlue.Height);
    end;
  end;

  PRGBColorOriginal.Color := RGBColorToColor(currentColor);

  if (PRGBColorOriginal.Color <> PRGBColor.Color) then
    IRGBRefresh.Visible := true
  else
    IRGBRefresh.Visible := false;
end;

procedure TFMain.PaintHSVView();
var
  I, colStep: Word;
  colTmp: TColorRef;
  changeEvent: TNotifyEvent;
begin
  PHSVColor.Color := HSVColorToColor(supHSVColor);

  {
    Block OnChange Events until set them all
  }
  changeEvent := SEHSVSetHue.OnChange;
  SEHSVSetHue.OnChange := nil;
  SEHSVSetSaturation.OnChange := nil;
  SEHSVSetValue.OnChange := nil;

  TBHSVSetHue.Position := supHSVColor.Hue;
  TBHSVSetSaturation.Position := supHSVColor.Saturation;
  TBHSVSetValue.Position := supHSVColor.Value;

  SEHSVSetHue.Value := supHSVColor.Hue;
  SEHSVSetSaturation.Value := supHSVColor.Saturation;
  SEHSVSetValue.Value := supHSVColor.Value;

  {
    Unblock onChange Events
  }
  SEHSVSetHue.OnChange := changeEvent;
  SEHSVSetSaturation.OnChange := changeEvent;
  SEHSVSetValue.OnChange := changeEvent;

  {
    Hue TrackBar Image
  }
  with IHSVHue.Canvas do
  begin
    Pen.Width := 1;
    Pen.Style := psSolid;
  end;
  colStep := Trunc(SEHSVSetHue.MaxValue / IHSVHue.Width);
  for I := 0 to IHSVHue.Width do begin
    colTmp := HSVToColor(I * colStep, SEHSVSetSaturation.Value, SEHSVSetValue.Value);
    with IHSVHue.Canvas do
    begin
      Pen.Color := colTmp;
      MoveTo(I, 0);
      LineTo(I, Height);
    end;
  end;

  {
    Saturation TrackBar Image
  }
  // TODO: in the future you can consider make trackbars longer than 100px
  //       but you have to rethought painting mechanizm and paint another
  //       color after few px or use temporary canvas
  with IHSVSaturation.Canvas do
  begin
    Pen.Width := 1;
    Pen.Style := psSolid;
  end;
  colStep := Trunc(SEHSVSetSaturation.MaxValue / IHSVSaturation.Width);
  for I := 0 to IHSVSaturation.Width do begin
    colTmp := HSVToColor(SEHSVSetHue.Value, I * colStep, SEHSVSetValue.Value);
    with IHSVSaturation.Canvas do
    begin
      Pen.Color := colTmp;
      MoveTo(I, 0);
      LineTo(I, Height);
    end;
  end;

  {
    Value TrackBar Image
  }
  with IHSVValue.Canvas do
  begin
    Pen.Width := 1;
    Pen.Style := psSolid;
  end;
  colStep := Trunc(SEHSVSetValue.MaxValue / IHSVValue.Width);
  for I := 0 to IRGBBlue.Width do begin
    colTmp := HSVToColor(SEHSVSetHue.Value, SEHSVSetSaturation.Value, I * colStep);
    with IHSVValue.Canvas do
    begin
      Pen.Color := colTmp;
      MoveTo(I, 0);
      LineTo(I, Height);
    end;
  end;

  PHSVColorOriginal.Color := HSVColorToColor(RGBColorToHSVColor(currentColor));

  if (PHSVColorOriginal.Color <> PHSVColor.Color) then
    IHSVRefresh.Visible := true
  else
    IHSVRefresh.Visible := false;
end;

function TFMain.GetAppDataDirectory(): String;
var
  Folder: array[0..MAX_PATH] of Char;
  appDataPath: String;
begin
  SHGetSpecialFolderPath(0, Folder, CSIDL_APPDATA , False);

  appDataPath := Folder;
  if (appDataPath <> '') then begin
    if (DirectoryExists(appDataPath + '\PAR Kit\') = False) then begin
      try
        CreateDir(appDataPath + '\PAR Kit\');
      except
        ShowMessage('Wystąpił błąd podczas tworzenia katalogu z ustawieniami.');
      end;
    end;

    appDataPath := appDataPath + '\PAR Kit\';
  end else
    appDataPath := ExtractFilePath(Application.ExeName);

  Result := appDataPath;
end;

{

  ++++++++++++++++++++ Events functions ++++++++++++++++++++

}
procedure TFMain.TrayIconClick(Sender: TObject);
begin
  if (FMain.Visible = True) then
    FMain.Visible := False
  else begin
    FMain.Visible := True;
    FMain.WindowState := wsNormal;
  end;
end;

procedure TFMain.TColorKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) then begin
    NewCurrentColor(StrToColor(TColor.Text, CBColorFormat.Text));
  end;
end;

procedure TFMain.PCurrentColorClick(Sender: TObject);
begin
  ColorDialog.Color := PCurrentColor.Color;
  if (ColorDialog.Execute) then begin
    NewCurrentColor(TColorToRGBColor(ColorDialog.Color));
  end;
end;

procedure TFMain.PHistColor0Click(Sender: TObject);
var
  tmpPName: String;
begin
  tmpPName := (Sender as TPanel).Name;
  Delete(tmpPName, 1, 10);

  if (StrToInt(tmpPName) < (colorsArray[colPointer].Undos)) then begin
    NewCurrentColor(TColorToRGBColor((Sender as TPanel).Color));
    NPickerTools.PageIndex := 0;
  end;
end;

procedure TFMain.SBHSVAbandonClick(Sender: TObject);
begin
  SetHSVColor(RGBColorToHSVColor(currentColor));

  NPickerTools.PageIndex := 0;
end;

procedure TFMain.SBHSVClick(Sender: TObject);
begin
  NPickerTools.PageIndex := 3;
end;

procedure TFMain.SBHSVConfirmClick(Sender: TObject);
begin
  NewCurrentColor(HSVColorToRGBColor(supHSVColor));

  NPickerTools.PageIndex := 0;
end;

procedure TFMain.SBRGBAbandonClick(Sender: TObject);
begin
  SetRGBColor(currentColor);

  NPickerTools.PageIndex := 0;
end;

procedure TFMain.SBRGBClick(Sender: TObject);
begin
  NPickerTools.PageIndex := 2;
end;

procedure TFMain.SBRGBConfirmClick(Sender: TObject);
begin
  NewCurrentColor(supRGBColor);

  NPickerTools.PageIndex := 0;
end;

procedure TFMain.SBUndoClick(Sender: TObject);
begin
  NPickerTools.PageIndex := 1;
end;

procedure TFMain.SBHistReturnClick(Sender: TObject);
begin
  NPickerTools.PageIndex := 0;
end;

procedure TFMain.SEHSVSetHueChange(Sender: TObject);
begin
  SetHSVColor(HSVToHSVColor(SEHSVSetHue.Value, SEHSVSetSaturation.Value, SEHSVSetValue.Value));
end;

procedure TFMain.SESetRedChange(Sender: TObject);
begin
  SetRGBColor(RGBToRGBColor(SESetRed.Value, SESetGreen.Value, SESetBlue.Value));
end;

procedure TFMain.TBHSVSetHueChange(Sender: TObject);
begin
  SEHSVSetHue.Value := TBHSVSetHue.Position;
end;

procedure TFMain.TBHSVSetSaturationChange(Sender: TObject);
begin
  SEHSVSetSaturation.Value := TBHSVSetSaturation.Position;
end;

procedure TFMain.TBHSVSetValueChange(Sender: TObject);
begin
  SEHSVSetValue.Value := TBHSVSetValue.Position;
end;

procedure TFMain.TBSetBlueChange(Sender: TObject);
begin
  SESetBlue.Value := TBSetBlue.Position;
end;

procedure TFMain.TBSetGreenChange(Sender: TObject);
begin
  SESetGreen.Value := TBSetGreen.Position;
end;

procedure TFMain.TBSetRedChange(Sender: TObject);
begin
  SESetRed.Value := TBSetRed.Position;
end;

procedure TFMain.TBTransparencyChange(Sender: TObject);
begin
  {$IFDEF Windows}
  if (TBTransparency.Position < 255) then
    AlphaBlend := true
  else
    AlphaBlend := false;
  AlphaBlendValue := TBTransparency.Position;
  LAlphaValue.Caption := IntToStr(100 - Round(100*TBTransparency.Position/255)) + '%';
  {$ENDIF}
end;

procedure TFMain.LAppWwwClick(Sender: TObject);
var
  v: THTMLBrowserHelpViewer;
  BrowserProcess: TProcessUTF8;
  BrowserPath, BrowserParams, URL: String;
  p: LongInt;
begin
  v := THTMLBrowserHelpViewer.Create(nil);
  try
    v.FindDefaultBrowser(BrowserPath,BrowserParams);
    debugln(['Path=', BrowserPath, ' Params=', BrowserParams]);

    if (Sender as TLabel) = LAppMail then
      URL:='mailto: admin@viman.com';
    if (Sender as TLabel) = LAppWww then
      URL:='http://viman.pl';
    p := System.Pos('%s', BrowserParams);
    System.Delete(BrowserParams, p, 2);
    System.Insert(URL, BrowserParams, p);

    // start browser
    BrowserProcess := TProcessUTF8.Create(nil);
    try
      BrowserProcess.CommandLine := BrowserPath + ' ' + BrowserParams;
      BrowserProcess.Execute;
    finally
      BrowserProcess.Free;
    end;
  finally
    v.Free;
  end;
end;

procedure TFMain.LAppWwwMouseEnter(Sender: TObject);
begin
  (Sender as TLabel).Font.Color := clRed;
end;

procedure TFMain.LAppWwwMouseLeave(Sender: TObject);
begin
  (Sender as TLabel).Font.Color := clBlue;
end;

procedure TFMain.LAppWwwMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  (Sender as TLabel).Font.Color := clRed;
end;

procedure TFMain.MICloseClick(Sender: TObject);
begin
  FMain.Close;
end;

procedure TFMain.MMICloseClick(Sender: TObject);
begin
  FMain.Close;
end;

procedure TFMain.MMISettingsClick(Sender: TObject);
begin
  NPages.PageIndex := 3;
end;

procedure TFMain.MMIInfoClick(Sender: TObject);
begin
  NInfo.PageIndex := 0;
  NPages.PageIndex := 0;
end;

procedure TFMain.MMIPickerClick(Sender: TObject);
begin
  PaintView();
  NPages.PageIndex := 1;
end;

procedure TFMain.MMISupportClick(Sender: TObject);
begin
  NInfo.PageIndex := 1;
  NPages.PageIndex := 0;
end;

procedure TFMain.MMIRulerClick(Sender: TObject);
begin
  NPages.PageIndex := 2;
end;

procedure TFMain.PCol0Click(Sender: TObject);
var
  tmpPName: String;
begin
  tmpPName := (Sender as TPanel).Name;
  Delete(tmpPName, 1, 4);
  try
    colPointer := StrToInt(tmpPName);

    SetCurrentColor(colorsArray[colPointer].Color);
  except
  end;
end;

procedure TFMain.PHSVColorClick(Sender: TObject);
begin
  ColorDialog.Color := PHSVColor.Color;
  if (ColorDialog.Execute) then begin
    SetHSVColor(TColorToHSVColor(ColorDialog.Color));
  end;
end;

procedure TFMain.PRGBColorClick(Sender: TObject);
begin
  ColorDialog.Color := PRGBColor.Color;
  if (ColorDialog.Execute) then begin
    SetRGBColor(TColorToRGBColor(ColorDialog.Color));
  end;
end;

procedure TFMain.BPickClick(Sender: TObject);
begin
  FMain.Visible := False;

  FDesktop := TFDesktop.Create(Application);

  FDesktop.PickerDelay := (TBPickDelay.Position * 1000) + 200;
  FDesktop.toolMode := 'PICKER';

  FDesktop.ShowModal;
  FDesktop.Free;

  FMain.Visible := True;
end;

procedure TFMain.BSetClick(Sender: TObject);
begin
  NewCurrentColor(StrToColor(TColor.Text, CBColorFormat.Text));
end;

procedure TFMain.CBColorFormatChange(Sender: TObject);
begin
  PaintView();
end;

procedure TFMain.CBLanguageChange(Sender: TObject);
var
  AProcess: TProcess;
// This is where our program starts to run
begin
  FMain.Close();

  // Now we will create the TProcess object, and
  // assign it to the var AProcess.
  AProcess := TProcess.Create(nil);

  // Tell the new AProcess what the command to execute is.
  // Let's use the FreePascal compiler
  AProcess.CommandLine := Application.ExeName;

  // We will define an option for when the program
  // is run. This option will make sure that our program
  // does not continue until the program we will launch
  // has stopped running.                vvvvvvvvvvvvvv
  //AProcess.Options := AProcess.Options + [poWaitOnExit];

  // Now that AProcess knows what the commandline is
  // we will run it.
  AProcess.Execute;

  // This is not reached until ppc386 stops running.
  AProcess.Free;
end;

procedure TFMain.CBOnTopChange(Sender: TObject);
begin
  if (CBOnTop.Checked = true) then
    {$IFDEF Windows}
    FMain.FormStyle := fsSystemStayOnTop
    {$ELSE}
    FMain.FormStyle := fsStayOnTop
    {$ENDIF}
  else
    FMain.FormStyle := fsNormal;
end;

procedure TFMain.CBShowTrayChange(Sender: TObject);
begin
  if (CBShowTray.Checked = true) then
    TrayIcon.Visible := true
  else
    TrayIcon.Visible := false;
end;

procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  INI: TIniFile;
begin
  try
    INI := TINIFile.Create(iniPath);

    INI.WriteString('App', 'Language', CBLanguage.Text);
    INI.WriteInteger('App', 'Top', FMain.Top);
    INI.WriteInteger('App', 'Left', FMain.Left);
    INI.WriteBool('App', 'TryIcon', CBShowTray.Checked);
    INI.WriteBool('App', 'MinimizeToTray', CBHideToTray.Checked);
    if (TBTransparency.Position < 255) then
      INI.WriteBool('App', 'Transparency', true)
    else
      INI.WriteBool('App', 'Transparency', false);
    INI.WriteInteger('App', 'TransparencyValue', TBTransparency.Position);
    INI.WriteBool('App', 'AlwaysOnTop', CBOnTop.Checked);
    INI.WriteString('Picker', 'ColorFormat', CBColorFormat.Text);
    INI.WriteBool('Picker', 'UseCapitalLetters', CBUseCapitalLetters.Checked);
    INI.WriteInteger('Picker', 'Delay', TBPickDelay.Position);
    INI.WriteBool('Picker', 'UseSafeColorsPalette', CBUseSafeWebColors.Checked);

    INI.Free;
  except
  end;
end;

procedure TFMain.FormCreate(Sender: TObject);
var
  I, J: Byte;
  initialColor: TRGBColor;
var
  INI: TIniFile;
begin
  with initialColor do begin
    Red   := 255;
    Green := 255;
    Blue  := 255;
  end;
  for I := 0 to High(colorsArray) do begin
    colorsArray[I].Used := false;
    colorsArray[I].Undos := 0;
    colorsArray[I].Color := initialColor;
    for J := 0 to High(colorsArray[I].History) do begin
      colorsArray[I].History[J] := initialColor;
    end;
  end;
  colPointer := 0;
  SetCurrentColor(initialColor);

  for I := 0 to High(AppLangs) do begin
    if (FileExists(FMain.GetAppDataDirectory() + 'languages/parkit.' + AppLangs[I][0] + '.po')) then
      CBLanguage.Items.Add(AppLangs[I][1]);
  end;

  iniPath := GetAppDataDirectory() + 'parkit.ini';

  try
    INI := TINIFile.Create(iniPath);

    CBLanguage.Text         := INI.ReadString('App', 'Language', 'Polski');
    FMain.Top               := INI.ReadInteger('App', 'Top', (Screen.Height - FMain.Height) div 2);
    FMain.Left              := INI.ReadInteger('App', 'Left', (Screen.Width - FMain.Width) div 2);
    TrayIcon.Visible        := INI.ReadBool('App', 'TryIcon', true);
    CBShowTray.Checked      := TrayIcon.Visible;
    CBHideToTray.Checked    := INI.ReadBool('App', 'MinimizeToTray', false);
    {$IFDEF Windows}
    FMain.AlphaBlend        := INI.ReadBool('App', 'Transparency', false);
    FMain.AlphaBlendValue   := INI.ReadInteger('App', 'TransparencyValue', 255);
    TBTransparency.Position := FMain.AlphaBlendValue;
    {$ENDIF}
    CBOnTop.Checked         := INI.ReadBool('App', 'AlwaysOnTop', true);
    if (CBOnTop.Checked = true) then
      {$IFDEF Windows}
      FMain.FormStyle := fsSystemStayOnTop
      {$ELSE}
      FMain.FormStyle := fsStayOnTop
      {$ENDIF}
    else
      FMain.FormStyle := fsNormal;
    CBColorFormat.Text          := INI.ReadString('Picker', 'ColorFormat', 'HTML');
    CBUseCapitalLetters.Checked := INI.ReadBool('Picker', 'UseCapitalLetters', false);
    TBPickDelay.Position        := INI.ReadInteger('Picker', 'Delay', 0);
    CBUseSafeWebColors.Checked  := INI.ReadBool('Picker', 'UseSafeColorsPalette', false);

    INI.Free;
  except
  end;

  PaintView();
end;

procedure TFMain.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  {$IFDEF Windows}
  if Button = mbLeft then begin
    ReleaseCapture;
    SendMessage(Handle, WM_NCLBUTTONDOWN, HTCAPTION, 0);
  end;
  {$ENDIF}
end;

procedure TFMain.IHSVRefreshClick(Sender: TObject);
begin
  SetHSVColor(RGBColorToHSVColor(currentColor));
end;

procedure TFMain.IRGBRefreshClick(Sender: TObject);
begin
  SetRGBColor(currentColor);
end;

procedure TFMain.ISafeColorClick(Sender: TObject);
begin
  NewCurrentColor(SafeWebColor(currentColor));
end;

procedure TFMain.BRulerClick(Sender: TObject);
begin
  FMain.Visible := False;

  FDesktop := TFDesktop.Create(Application);

  FDesktop.PickerDelay := (TBPickDelay.Position * 1000) + 200;
  FDesktop.toolMode := 'RULER';

  FDesktop.ShowModal;
  FDesktop.Free;

  FMain.Visible := True;
end;

procedure TFMain.BCopyClick(Sender: TObject);
begin
  Clipboard.AsText := TColor.Text;
end;

procedure TFMain.ApplicationPropertiesMinimize(Sender: TObject);
begin
  if ((CBHideToTray.Checked = true) AND (TrayIcon.Visible = true)) then begin
    FMain.WindowState := wsNormal;
    FMain.Visible := false;
  end;
end;

end.
