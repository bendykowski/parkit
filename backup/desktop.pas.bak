unit desktop;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF Windows}Windows,{$ELSE}LCLIntf, LCLType,{$ENDIF} SysUtils, Variants,
  Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Math,
  ColorUtils, StrUtils;

type

  { TFDesktop }

  TFDesktop = class(TForm)
    BExit: TButton;
    PBalloon: TPanel;
    IZoom: TImage;
    PColor: TPanel;
    LPosition: TLabel;
    LColor: TLabel;
    PMeasure: TPanel;
    GBCoordinates: TGroupBox;
    GBMeasurement: TGroupBox;
    LBegCaption: TLabel;
    LEndCaption: TLabel;
    LBegCoordinates: TLabel;
    LEndCoordinates: TLabel;
    LWidthCaption: TLabel;
    LHeightCaption: TLabel;
    LDiagonalCaption: TLabel;
    LHeight: TLabel;
    LWidth: TLabel;
    LDiagonal: TLabel;
    ILeft: TImage;
    IBottom: TImage;
    IScreen: TImage;
    procedure BExitClick(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PBScreenClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure PMeasureMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PMeasureClick(Sender: TObject);

    procedure SetZoomParameters();

    procedure PaintRuler();
    procedure PaintBalloon();
    procedure PaintScreen();

    procedure ResetView();
    procedure UpdateView();

    function MouseInArea(const startX, startY, stopX, stopY: Integer; const cursorPos: TPoint): Boolean;
  private
    { private declarations }
  public
    pickerDelay: Word;

    screenPicture: TBitmap;
    screenDC: HDC;

    zoomScreenSize, zoomPixelSize: Byte;
    zoomLevel, zoomPointer: Byte;
    prePadding, postPadding: Byte;
    zoomRect, zoomScreenRect: TRect;
    balloonHeight: Word;

    toolMode: string;
    selStartPoint, selStopPoint: TPoint;
    makeMeasure, isSelected: boolean;
    moveSelection: boolean;
    movePoint: TPoint;
    moveType: Integer;

    boostPrecision: boolean;
    currentPos, tempPos: TPoint;
  end;

var
  FDesktop: TFDesktop;

const
  zoomLevels: array[0..2] of Byte = (5, 10, 20);

  // movement points
  moveStartPoint       = $0000;
  moveStartXWall       = $0001;
  moveStartYWall       = $0002;
  moveStartYStopXPoint = $0003;
  moveStopPoint        = $0004;
  moveStopXWall        = $0005;
  moveStopYWall        = $0006;
  moveStopYStartXPoint = $0007;
  moveWhole            = $0008;


implementation

uses
  main;

{$R *.lfm}

procedure TFDesktop.ResetView();
begin
  isSelected     := False;
  moveSelection  := False;
  movePoint      := Point(0, 0);
  moveType       := -1;
  selStartPoint  := Point(0, 0);
  selStopPoint   := Point(0, 0);
  IScreen.Cursor := crDefault;
end;

procedure TFDesktop.SetZoomParameters();
begin
  zoomLevel := zoomLevels[zoomPointer];

  zoomScreenSize := Round(IZoom.Width / zoomLevel);
  zoomPixelSize := zoomLevel;

  prePadding := Floor(zoomScreenSize / 2);
  postPadding := Ceil(zoomScreenSize / 2);
end;

procedure TFDesktop.PaintScreen();
begin
  // it's a direct way to copy, canvas.draw uses this
  BitBlt(IScreen.Canvas.Handle, 0, 0, IScreen.Width, IScreen.Height,
    screenPicture.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TFDesktop.UpdateView();
begin
  PBalloon.Update;
  IScreen.Update;
  FDesktop.Update;
end;

procedure TFDesktop.PaintBalloon();
var
  middleX, middleY: shortint;
  helpLines: Word;
  cursorPos: TPoint;
  I: Integer;
begin
  GetCursorPos(cursorPos);

  IZoom.Canvas.Brush.Color := $00553929;
  IZoom.Canvas.FillRect(zoomRect);

  zoomScreenRect := Rect(cursorPos.X - prePadding, cursorPos.Y - prePadding,
    cursorPos.X + postPadding, cursorPos.Y + postPadding);
  if (isSelected = True) then
    IZoom.Canvas.CopyRect(zoomRect, IScreen.Canvas, zoomScreenRect)
  else
    IZoom.Canvas.CopyRect(zoomRect, screenPicture.Canvas, zoomScreenRect);

  if (cursorPos.X + PBalloon.Width + 15 > Screen.Width) then
    PBalloon.Left := cursorPos.X - PBalloon.Width - 10
  else
  begin
    if ((toolMode = 'RULER') and (makeMeasure = True) and
      (selStartPoint.X > selStopPoint.X) and (selStartPoint.Y > selStopPoint.Y) and
      not (cursorPos.X - PBalloon.Width - 15 < 0)) then
      PBalloon.Left := cursorPos.X - PBalloon.Width - 10
    else
      PBalloon.Left := cursorPos.X + 10;
  end;

  if (cursorPos.Y + PBalloon.Height + 15 > Screen.Height) then
    PBalloon.Top := cursorPos.Y - PBalloon.Height - 10
  else
    PBalloon.Top := cursorPos.Y + 10;

  if (toolMode = 'RULER') then
  begin
    PColor.Visible := False;
    LColor.Visible := False;
    LPosition.Top := balloonHeight - (2 * PColor.Height) - 4;
    PBalloon.Height := balloonHeight - PColor.Height - 2;
  end
  else if (toolMode = 'PICKER') then
  begin
    PColor.Visible := True;
    LColor.Visible := True;
    LColor.Top := balloonHeight - PColor.Height - 2;
    LPosition.Top := balloonHeight - PColor.Height - 2;
    PBalloon.Height := balloonHeight;

    if (FMain.CBUseSafeWebColors.Checked = false) then begin
      PColor.Color := IScreen.Canvas.Pixels[cursorPos.X, cursorPos.Y];
      LColor.Caption := AnsiReplaceText(
                          ColorToStr(TColorToRGBColor(PColor.Color),
                                     FMain.CBColorFormat.Text,
                                     FMain.CBUseCapitalLetters.Checked),
                          '&',
                          '&&');
    end else begin
      PColor.Color := RGBColorToColor(SafeWebColor(TColorToRGBColor(IScreen.Canvas.Pixels[cursorPos.X, cursorPos.Y])));
      LColor.Caption := AnsiReplaceText(
                          ColorToStr(SafeWebColor(TColorToRGBColor(PColor.Color)),
                                     FMain.CBColorFormat.Text,
                                     FMain.CBUseCapitalLetters.Checked),
                          '&',
                          '&&');
    end;

    if (LColor.Width + LPosition.Width + 4 > PBalloon.Width) then
      LPosition.Visible := False
    else
      LPosition.Visible := True;
  end;

  LPosition.Caption := '(' + IntToStr(cursorPos.X) + ',' + IntToStr(cursorPos.Y) + ')';

  middleX := ((zoomScreenSize div 2) * zoomPixelSize);
  middleY := ((zoomScreenSize div 2) * zoomPixelSize);

  with IZoom.Canvas do
  begin
    // we can to reduce it into only one for because it's square shape
    for I := middleX - zoomPixelSize to middleX + 2 * zoomPixelSize - 1 do begin
      // x
      Pixels[I, middleY - 1] := InvertColor(Pixels[I, middleY - 1]);
      Pixels[I, middleY + zoomPixelSize] := InvertColor(Pixels[I, middleY + zoomPixelSize]);

      // y
      Pixels[middleX - 1, I] := InvertColor(Pixels[middleX - 1, I]);
      Pixels[middleX + zoomPixelSize, I] := InvertColor(Pixels[middleX + zoomPixelSize, I]);
    end;

    Pen.Color := clBlack;
    Pen.Width := 1;

    if (toolMode = 'RULER') then
    begin
      helpLines := 5;

      for I := 0 to helpLines do begin
        // top
        Pixels[middleX - 1, I] := InvertColor(Pixels[middleX - 1, I]);
        Pixels[middleX + zoomPixelSize, I] := InvertColor(Pixels[middleX + zoomPixelSize, I]);

        // right
        Pixels[IZoom.Width - I - 1, middleY - 1] := InvertColor(Pixels[IZoom.Width - I - 1, middleY - 1]);
        Pixels[IZoom.Width - I - 1, middleY + zoomPixelSize] := InvertColor(Pixels[IZoom.Width - I - 1, middleY + zoomPixelSize]);

        //bottom
        Pixels[middleX - 1, IZoom.Width - I - 1] := InvertColor(Pixels[middleX - 1, IZoom.Width - I - 1]);
        Pixels[middleX + zoomPixelSize, IZoom.Width - I - 1] := InvertColor(Pixels[middleX + zoomPixelSize, IZoom.Width - I - 1]);

        // left
        Pixels[I, middleY - 1] := InvertColor(Pixels[I, middleY - 1]);
        Pixels[I, middleY + zoomPixelSize] := InvertColor(Pixels[I, middleY + zoomPixelSize]);
      end;
    end;
  end;

  ILeft.Top := cursorPos.Y - Round(ILeft.Height / 2);
  IBottom.Left := cursorPos.X - Round(IBottom.Width / 2);

  PBalloon.BringToFront;
  PMeasure.SendToBack;
end;

procedure TFDesktop.PaintRuler();
var
  selWidth, selHeight, selDiagonal: Float;
  selWidthCaption, selHeightCaption, selDiagonalCaption: string;
  wWidth, hWidth, wHeight, hHeight, wDiagonal, hDiagonal: Byte;
  maxX, minY: Word;
  cursorPos: TPoint;
begin
  GetCursorPos(cursorPos);

  selWidth := Abs(selStartPoint.X - selStopPoint.X) + 1;
  selHeight := Abs(selStartPoint.Y - selStopPoint.Y) + 1;
  selDiagonal := Round(Sqrt(Sqr(Abs(selStartPoint.X - selStopPoint.X) + 1) +
    Sqr(Abs(selStartPoint.Y - selStopPoint.Y) + 1)));

  selWidthCaption := FloatToStr(selWidth) + 'px';
  selHeightCaption := FloatToStr(selHeight) + 'px';
  selDiagonalCaption := FloatToStr(selDiagonal) + 'px';

  maxX := Max(selStartPoint.X, selStopPoint.X);
  minY := Min(selStartPoint.Y, selStopPoint.Y);

  with IScreen.Canvas do
  begin
    // diagonal
    Pen.Width := 1;
    Pen.Color := clRed;
    Pen.Style := psDash;
    MoveTo(selStartPoint.X, selStartPoint.Y);
    LineTo(selStopPoint.X, selStopPoint.Y);

    // rectangle
    Pen.Color := clBlue;
    MoveTo(selStartPoint.X, selStartPoint.Y);
    LineTo(selStopPoint.X, selStartPoint.Y);
    LineTo(selStopPoint.X, selStopPoint.Y);
    LineTo(selStartPoint.X, selStopPoint.Y);
    LineTo(selStartPoint.X, selStartPoint.Y);

    Font.Name := 'Tahoma';
    Font.Size := 7;
    Font.Color := clBlue;

    wWidth := TextWidth(selWidthCaption);
    hWidth := TextHeight(selWidthCaption);
    wHeight := TextWidth(selHeightCaption);
    hHeight := TextHeight(selHeightCaption);
    wDiagonal := TextWidth(selDiagonalCaption);
    hdiagonal := TextHeight(selDiagonalCaption);

    { vertical caption }
    if (MinY - hWidth - 5 < 0) then
      TextOut(selStopPoint.X + ((selStartPoint.X - selStopPoint.X - wWidth) div 2),
              MinY + 5,
              selWidthCaption)
    else
      TextOut(selStopPoint.X + ((selStartPoint.X - selStopPoint.X - wWidth) div 2),
              MinY - hWidth - 5,
              selWidthCaption);

    { horizontal caption }
    if (MaxX + wHeight + 10 > Screen.Width) then
      TextOut(MaxX - wHeight - 5,
              selStopPoint.Y + ((selStartPoint.Y - selStopPoint.Y - hHeight) div 2),
              selHeightCaption)
    else
      TextOut(MaxX + 5,
              selStopPoint.Y + ((selStartPoint.Y - selStopPoint.Y - hHeight) div 2),
              selHeightCaption);

    { diagonal caption }
    if ((selWidth >= wDiagonal + 6)
         AND (selHeight >= TextHeight(selDiagonalCaption) + 4)) then
    begin
      Font.Color := clRed;
      TextOut(selStopPoint.X + ((selStartPoint.X - selStopPoint.X - wDiagonal) div 2), selStopPoint.Y + ((selStartPoint.Y - selStopPoint.Y - hDiagonal) div 2), selDiagonalCaption);
    end;
  end;
end;

function TFDesktop.MouseInArea(const startX, startY, stopX, stopY: Integer; const cursorPos: TPoint): Boolean;
begin
  if ((cursorPos.X > startX)
       AND (cursorPos.X < stopX)
       AND (cursorPos.Y > startY)
       AND (cursorPos.Y < stopY)) then
    Result := True
  else
    Result := False;
end;

procedure TFDesktop.FormCreate(Sender: TObject);
begin
  SetWindowPos(Handle, HWND_TOPMOST, Left, Top, Width, Height, SWP_NOSIZE or SWP_NOMOVE);
end;

procedure TFDesktop.FormDestroy(Sender: TObject);
begin
  screenPicture.Free;
end;

procedure TFDesktop.FormKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = '+') then
  begin // powiększ
    if (zoomPointer < High(zoomLevels)) then
    begin
      zoomPointer := zoomPointer + 1;

      SetZoomParameters();
      PaintBalloon();
      UpdateView();
    end;
  end
  else if (Key = '-') then
  begin // pomniejsz
    if (zoomPointer > Low(zoomLevels)) then
    begin
      zoomPointer := zoomPointer - 1;

      SetZoomParameters();
      PaintBalloon();
      UpdateView();
    end;
  end;
end;

procedure TFDesktop.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  cursorPos: TPoint;
  selMargin: Byte = 5;
  minX, minY, maxX, maxY: Word;
begin
  GetCursorPos(cursorPos);

  if (Button = MBRIGHT) then
  begin
    if (boostPrecision = false) then begin
      currentPos := cursorPos;
      tempPos := cursorPos;
      boostPrecision := true;
    end
    else begin
      boostPrecision := false;
    end;
  end
  else if (toolMode = 'RULER') then
  begin
    PMeasure.Visible := False;

    if (Button = MBMIDDLE) then
    begin
      ResetView();

      PaintScreen();
      PaintBalloon();

      toolMode := 'PICKER';
    end else if (Button = MBLEFT) then begin
      if (isSelected = true) then begin
        minX := Min(selStartPoint.X, selStopPoint.X);
        minY := Min(selStartPoint.Y, selStopPoint.Y);
        maxX := Max(selStartPoint.X, selStopPoint.X);
        maxY := Max(selStartPoint.Y, selStopPoint.Y);

        // moveWhole
        if (MouseInArea(minX + selMargin,
                        minY + selMargin,
                        maxX - selMargin,
                        maxY - selMargin,
                        cursorPos) = True) then begin
          moveSelection := true;
          moveType := moveWhole;
          movePoint := cursorPos;

          IScreen.Cursor := crSizeAll;
        // moveStartPoint
        end else if (MouseInArea(selStartPoint.X - selMargin,
                                 selStartPoint.Y - selMargin,
                                 selStartPoint.X + selMargin,
                                 selStartPoint.Y + selMargin,
                                 cursorPos) = True) then begin
          moveSelection := true;
          moveType := moveStartPoint;
          movePoint := cursorPos;

          if (((selStartPoint.X = minX)
                AND (selStartPoint.Y = minY))
              OR ((selStartPoint.X = maxX)
                AND (selStartPoint.Y = maxY))) then
            IScreen.Cursor := crSizeNWSE
          else
            IScreen.Cursor := crSizeNESW;

        // moveStartXWall
        end else if (MouseInArea(minX + selMargin,
                                 selStartPoint.Y - selMargin,
                                 maxX - selMargin,
                                 selStartPoint.Y + selMargin,
                                 cursorPos) = True) then begin
          moveSelection := true;
          moveType := moveStartXWall;
          movePoint := cursorPos;

          IScreen.Cursor := crSizeNS;
        // moveStartYWall
        end else if (MouseInArea(selStartPoint.X - selMargin,
                                 minY + selMargin,
                                 selStartPoint.X + selMargin,
                                 maxY - selMargin,
                                 cursorPos) = True) then begin

          moveSelection := true;
          moveType := moveStartYWall;
          movePoint := cursorPos;

          IScreen.Cursor := crSizeWE;
        // moveStopPoint
        end else if (MouseInArea(selStopPoint.X - selMargin,
                                 selStopPoint.Y - selMargin,
                                 selStopPoint.X + selMargin,
                                 selStopPoint.Y + selMargin,
                                 cursorPos) = True) then begin
          moveSelection := true;
          moveType := moveStopPoint;
          movePoint := cursorPos;

          if (((selStopPoint.X = minX)
                AND (selStopPoint.Y = minY))
              OR ((selStopPoint.X = maxX)
                AND (selStopPoint.Y = maxY))) then
            IScreen.Cursor := crSizeNWSE
          else
            IScreen.Cursor := crSizeNESW;
        // moveStopXWall
        end else if (MouseInArea(minX + selMargin,
                                 selStopPoint.Y - selMargin,
                                 maxX - selMargin,
                                 selStopPoint.Y + selMargin,
                                 cursorPos) = True) then begin
          moveSelection := true;
          moveType := moveStopXWall;
          movePoint := cursorPos;

          IScreen.Cursor := crSizeNS;
        // moveStartYWall
        end else if (MouseInArea(selStopPoint.X - selMargin,
                                 minY + selMargin,
                                 selStopPoint.X + selMargin,
                                 maxY - selMargin,
                                 cursorPos) = True) then begin
          moveSelection := true;
          moveType := moveStopYWall;
          movePoint := cursorPos;

          IScreen.Cursor := crSizeWE;
        // moveStartYStopXPoint
        end else if (MouseInArea(selStartPoint.X - selMargin,
                                 selStopPoint.Y - selMargin,
                                 selStartPoint.X + selMargin,
                                 selStopPoint.Y + selMargin,
                                 cursorPos) = True) then begin
          moveSelection := true;
          moveType := moveStartYStopXPoint;
          movePoint := cursorPos;

          if (((selStopPoint.X = minX)
                AND (selStartPoint.Y = minY))
              OR ((selStopPoint.X = maxX)
                AND (selStartPoint.Y = maxY))) then
            IScreen.Cursor := crSizeNWSE
          else
            IScreen.Cursor := crSizeNESW;
        // moveStopYStartXPoint
        end else if (MouseInArea(selStopPoint.X - selMargin,
                                 selStartPoint.Y - selMargin,
                                 selStopPoint.X + selMargin,
                                 selStartPoint.Y + selMargin,
                                 cursorPos) = True) then begin
          moveSelection := true;
          moveType := moveStopYStartXPoint;
          movePoint := cursorPos;

          if (((selStopPoint.X = minX)
                AND (selStartPoint.Y = minY))
              OR ((selStopPoint.X = maxX)
                AND (selStartPoint.Y = maxY))) then
            IScreen.Cursor := crSizeNWSE
          else
            IScreen.Cursor := crSizeNESW;
        end else begin
          isSelected := false;
          makeMeasure := True;

          selStartPoint := cursorPos;
        end;
      end else begin
        makeMeasure := True;

        selStartPoint := cursorPos;
      end;
    end;
  end
  else if (toolMode = 'PICKER') then
  begin
    if (Button = MBMIDDLE) then
    begin
      ResetView();

      toolMode := 'RULER';
    end;
  end;

  UpdateView();
end;

procedure TFDesktop.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  cursorPos: TPoint;
  selMargin: Byte = 5;
  minX, minY, maxX, maxY: Word;
begin
  GetCursorPos(cursorPos);

  if (boostPrecision = true) then begin
    tempPos.X := tempPos.X + (cursorPos.X - currentPos.X);
    tempPos.Y := tempPos.Y + (cursorPos.Y - currentPos.Y);
    //tempPos := cursorPos;

    if (Abs(tempPos.X - currentPos.X) > zoomPixelSize) then begin
      currentPos.X := currentPos.X + Round(((tempPos.X - currentPos.X) / zoomPixelSize));
      tempPos.X := currentPos.X;
    end;

    if (Abs(tempPos.Y - currentPos.Y) > zoomPixelSize) then begin
      currentPos.Y := currentPos.Y + Round(((tempPos.Y - currentPos.Y) / zoomPixelSize));
      tempPos.Y := currentPos.Y;
    end;

    SetCursorPos(currentPos.X, currentPos.Y);
  end;

  if (toolMode = 'RULER') then
  begin
    if (moveSelection) then
    begin
      case moveType of
        moveWhole: begin
          selStartPoint.X := selStartPoint.X + cursorPos.X - movePoint.X;
          selStartPoint.Y := selStartPoint.Y + cursorPos.Y - movePoint.Y;
          selStopPoint.X := selStopPoint.X + cursorPos.X - movePoint.X;
          selStopPoint.Y := selStopPoint.Y + cursorPos.Y - movePoint.Y;

          if (Max(selStartPoint.X, selStopPoint.X) >= Screen.Width) then begin
            if (selStartPoint.X < selStopPoint.X) then begin
              selStartPoint.X := selStartPoint.X + (Screen.Width - 1 - selStopPoint.X);
              selStopPoint.X := Screen.Width - 1;
            end else begin
              selStopPoint.X := selStopPoint.X + (Screen.Width - 1 - selStartPoint.X);
              selStartPoint.X := Screen.Width - 1;
            end;
          end;

          if (Max(selStartPoint.Y, selStopPoint.Y) >= Screen.Height) then begin
            if (selStartPoint.Y < selStopPoint.Y) then begin
              selStartPoint.Y := selStartPoint.Y + (Screen.Height - 1 - selStopPoint.Y);
              selStopPoint.Y := Screen.Height - 1;
            end else begin
              selStopPoint.Y := selStopPoint.Y + (Screen.Height - 1 - selStartPoint.Y);
              selStartPoint.Y := Screen.Height - 1;
            end;
          end;

          if (Min(selStartPoint.X, selStopPoint.X) < 0) then begin
            if (selStartPoint.X < selStopPoint.X) then begin
              selStopPoint.X := selStopPoint.X - selStartPoint.X;
              selStartPoint.X := 0;
            end else begin
              selStartPoint.X := selStopPoint.X - selStartPoint.X;
              selStopPoint.X := 0;
            end;
          end;

          if (Min(selStartPoint.Y, selStopPoint.Y) < 0) then begin
            if (selStartPoint.Y < selStopPoint.Y) then begin
              selStopPoint.Y := selStopPoint.Y - selStartPoint.Y;
              selStartPoint.Y := 0;
            end else begin
              selStartPoint.Y := selStopPoint.Y - selStartPoint.Y;
              selStopPoint.Y := 0;
            end;
          end;

          movePoint := cursorPos;
        end;
        moveStartPoint: begin
          selStartPoint := cursorPos;
        end;
        moveStartXWall: begin
          selStartPoint.Y := cursorPos.Y;
        end;
        moveStartYWall: begin
          selStartPoint.X := cursorPos.X;
        end;
        moveStopPoint: begin
          selStopPoint := cursorPos;
        end;
        moveStopXWall: begin
          selStopPoint.Y := cursorPos.Y;
        end;
        moveStopYWall: begin
          selStopPoint.X := cursorPos.X;
        end;
        moveStartYStopXPoint: begin
          selStartPoint.X := cursorPos.X;
          selStopPoint.Y := cursorPos.Y;
        end;
        moveStopYStartXPoint: begin
          selStopPoint.X := cursorPos.X;
          selStartPoint.Y := cursorPos.Y;
        end;
      end;

      PaintScreen();
      PaintRuler();
    end else if (makeMeasure = True) then
    begin
      selStopPoint := cursorPos;

      PaintScreen();
      PaintRuler();
    end else if (isSelected = True) then
    begin
      minX := Min(selStartPoint.X, selStopPoint.X);
      minY := Min(selStartPoint.Y, selStopPoint.Y);
      maxX := Max(selStartPoint.X, selStopPoint.X);
      maxY := Max(selStartPoint.Y, selStopPoint.Y);

      // upper-left
      if (MouseInArea(minX - selMargin,
                      minY - selMargin,
                      minX + selMargin,
                      minY + selMargin,
                      cursorPos) = True) then begin
        IScreen.Cursor := crSizeNWSE;
      // upper
      end else if (MouseInArea(minX + selMargin,
                               minY - selMargin,
                               maxX - selMargin,
                               minY + selMargin,
                               cursorPos) = True) then begin
        IScreen.Cursor := crSizeNS;
      // upper-right
      end else if (MouseInArea(maxX - selMargin,
                               minY - selMargin,
                               maxX + selMargin,
                               minY + selMargin,
                               cursorPos) = True) then begin
        IScreen.Cursor := crSizeNESW;
      // left
      end else if (MouseInArea(minX - selMargin,
                               minY + selMargin,
                               minX + selMargin,
                               maxY - selMargin,
                               cursorPos) = True) then begin
        IScreen.Cursor := crSizeWE;
      // middle
      end else if (MouseInArea(minX + selMargin,
                               minY + selMargin,
                               maxX - selMargin,
                               maxY - selMargin,
                               cursorPos) = True) then begin
        IScreen.Cursor := crSize;
      // right
      end else if (MouseInArea(maxX - selMargin,
                               minY + selMargin,
                               maxX + selMargin,
                               maxY - selMargin,
                               cursorPos) = True) then begin
        IScreen.Cursor := crSizeWE;
      // lower-left
      end else if (MouseInArea(minX - selMargin,
                               maxY - selMargin,
                               minX + selMargin,
                               maxY + selMargin,
                               cursorPos) = True) then begin
        IScreen.Cursor := crSizeNESW;
      // lower
      end else if (MouseInArea(minX + selMargin,
                               maxY - selMargin,
                               maxX - selMargin,
                               maxY + selMargin,
                               cursorPos) = True) then begin
        IScreen.Cursor := crSizeNS;
      // lower-right
      end else if (MouseInArea(maxX - selMargin,
                               maxY - selMargin,
                               maxX + selMargin,
                               maxY + selMargin,
                               cursorPos) = True) then begin
        IScreen.Cursor := crSizeNWSE;
      // default
      end else
        IScreen.Cursor := crDefault;
    end;
  end;

  if (cursorPos.X - ILeft.Width <= 0) then
    ILeft.Visible := False
  else
    ILeft.Visible := True;
  if (cursorPos.Y + IBottom.Height >= Screen.Height) then
    IBottom.Visible := False
  else
    IBottom.Visible := True;

  PaintBalloon();
  UpdateView();
end;

procedure TFDesktop.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  cursorPos: TPoint;
  pixelColor: TRGBColor;
  selWidth, selHeight, selDiagonal: Word;
  selWidthCaption, selHeightCaption, selDiagonalCaption: string;
begin
  GetCursorPos(cursorPos);

  if (toolMode = 'RULER') then
  begin
    if (makeMeasure = True) then begin
      GetCursorPos(cursorPos);

      selStopPoint := cursorPos;
    end;

    if ((Button = MBLEFT) AND ((makeMeasure = True) OR (moveSelection = True))) then
    begin
      moveSelection := false;
      IScreen.Cursor := crDefault;

      if (selStartPoint.X <> selStopPoint.X) OR (selStartPoint.Y <> selStopPoint.Y) then begin
        isSelected := True;

        PaintScreen();
        PaintRuler();

        selWidth := Abs(selStartPoint.X - selStopPoint.X) + 1;
        selHeight := Abs(selStartPoint.Y - selStopPoint.Y) + 1;
        selDiagonal := Round(Sqrt(Sqr(Abs(selStartPoint.X - selStopPoint.X) + 1) +
          Sqr(Abs(selStartPoint.Y - selStopPoint.Y) + 1)));

        selWidthCaption := FloatToStr(selWidth) + 'px';
        selHeightCaption := FloatToStr(selHeight) + 'px';
        selDiagonalCaption := FloatToStr(selDiagonal) + 'px';

        LBegCoordinates.Caption :=
          '(' + IntToStr(selStartPoint.X) + ',' + IntToStr(selStartPoint.Y) + ')';
        LEndCoordinates.Caption :=
          '(' + IntToStr(selStopPoint.X) + ',' + IntToStr(selStopPoint.Y) + ')';
        LWidth.Caption := selWidthCaption;
        LHeight.Caption := selHeightCaption;
        LDiagonal.Caption := selDiagonalCaption;

        if (Min(selStartPoint.X, selStopPoint.X) > PMeasure.Width + 10) then
        begin
          PMeasure.Left := Min(selStartPoint.X, selStopPoint.X) - PMeasure.Width - 5;
          if (Min(selStartPoint.Y, selStopPoint.Y) + PMeasure.Height + 10 < IScreen.Height) then
            PMeasure.Top := Min(selStartPoint.Y, selStopPoint.Y)
          else
            PMeasure.Top := Min(selStartPoint.Y, selStopPoint.Y) - PMeasure.Height;
        end
        else if (Screen.Width - Max(selStartPoint.X, selStopPoint.X) >
          PMeasure.Width + 10) then
        begin
          PMeasure.Left := Max(selStartPoint.X, selStopPoint.X) + 5;
          if (Min(selStartPoint.Y, selStopPoint.Y) + PMeasure.Height + 10 < IScreen.Height) then
            PMeasure.Top := Min(selStartPoint.Y, selStopPoint.Y)
          else
            PMeasure.Top := Min(selStartPoint.Y, selStopPoint.Y) - PMeasure.Height;
          //PMeasure.Top := Min(selStartPoint.Y, selStopPoint.Y);
        end
        else if (Min(selStartPoint.Y, selStopPoint.Y) > PMeasure.Height + 10) then
        begin
          PMeasure.Left := Min(selStartPoint.X, selStopPoint.X);
          PMeasure.Top := Min(selStartPoint.Y, selStopPoint.Y) - PMeasure.Height - 5;
        end
        else if (Screen.Height - Max(selStartPoint.Y, selStopPoint.Y) >
          PMeasure.Height + 10) then
        begin
          PMeasure.Left := Min(selStartPoint.X, selStopPoint.X);
          PMeasure.Top := Max(selStartPoint.Y, selStopPoint.Y) + 5;
        end
        else
        begin
          PMeasure.Left := Max(selStartPoint.X, selStopPoint.X) - PMeasure.Width - 5;
          PMeasure.Top := Min(selStartPoint.Y, selStopPoint.Y) + 5;
        end;

        PMeasure.Visible := True;

        makeMeasure := False;
      end else begin
        PaintScreen();
        UpdateView();

        PMeasure.Visible := False;

        makeMeasure := False;
      end;
    end;
  end
  else if (toolMode = 'PICKER') then
  begin
    if (Button = MBLEFT) then
    begin
      pixelColor := TColorToRGBColor(IScreen.Canvas.Pixels[cursorPos.X, cursorPos.Y]);

      FMain.NewCurrentColor(pixelColor);

      FMain.NPages.PageIndex := 1;

      FDesktop.Close;
    end;
  end;

  PaintBalloon();

  UpdateView();
end;

procedure TFDesktop.BExitClick(Sender: TObject);
begin
  FMain.LWidth.Caption := LWidth.Caption;
  FMain.LHeight.Caption := LHeight.Caption;
  FMain.LDiagonal.Caption := LDiagonal.Caption;
  FMain.LBPoint.Caption := LBegCoordinates.Caption;
  FMain.LEPoint.Caption := LEndCoordinates.Caption;

  FMain.NPages.PageIndex := 2;

  FDesktop.Close;
end;

procedure TFDesktop.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  if (WheelDelta < 0) then
  begin // powiększ
    if (zoomPointer < High(zoomLevels)) then
      zoomPointer := zoomPointer + 1;
  end
  else if (WheelDelta > 0) then
  begin // pomniejsz
    if (zoomPointer > Low(zoomLevels)) then
      zoomPointer := zoomPointer - 1;
  end;

  SetZoomParameters();
  PaintBalloon();
  UpdateView();
end;

procedure TFDesktop.PMeasureMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  PMeasure.Visible := False;
end;

procedure TFDesktop.FormPaint(Sender: TObject);
begin
  SetZOrder(True);

  { draw screen on a form }
  //IScreen.Canvas.StretchDraw(Rect(0, 0, Width, Height), screenPicture);
end;

procedure TFDesktop.FormShow(Sender: TObject);
var
  cursorPos: TPoint;
begin
  Left := 0;
  Top := 0;
  Width := Screen.Width;
  Height := Screen.Height;

  Sleep(PickerDelay);

  Windows.SetForegroundWindow(FDesktop.Handle);

  screenPicture := TBitmap.Create;
  screenPicture.Width := Screen.Width;
  screenPicture.Height := Screen.Height;
  screenPicture := FMain.CaptureDesktop();

  { paint IScreen with dekstop print screen }
  PaintScreen();

  { set positions of arrows }
  ILeft.Left := 0;
  IBottom.Top := Screen.Height - IBottom.Height;

  zoomPointer := 0;
  SetZoomParameters();
  zoomRect := Rect(0, 0, IZoom.Width, IZoom.Height);

  PMeasure.Top := 5;
  PMeasure.Left := 5;
  makeMeasure := False;

  balloonHeight := PBalloon.Height;

  DoubleBuffered := true;

  GetCursorPos(cursorPos);
  currentPos := cursorPos;
  tempPos := cursorPos;
  boostPrecision := false;

  PaintBalloon();
  UpdateView();
end;

procedure TFDesktop.PBScreenClick(Sender: TObject);
begin
  FDesktop.Close;
end;

procedure TFDesktop.PMeasureClick(Sender: TObject);
begin
  PMeasure.Visible := False;
end;

end.

