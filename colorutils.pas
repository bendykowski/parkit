unit ColorUtils;

{$mode objfpc}{$H+}

interface

uses
  Graphics, {$IFDEF Windows}Windows,{$ELSE}LCLIntf, LCLType,{$ENDIF} Math,
  SysUtils, Classes, StrUtils, Forms, Dialogs, RegExpr;

type
  THSVColor = record
    Hue: word;        // range(0..360)
    Saturation: byte; // range(0..100)
    Value: byte;      // range(0..100)
  end;

  {
  THSLColor = record
    Hue: word;        // range(0..360)
    Saturation: byte; // range(0..100)
    Lightness: byte;  // range(0..100)
  end;
  }

  TRGBColor = record
    Red: byte;        // range(0..255)
    Green: byte;      // range(0..255)
    Blue: byte;       // range(0..255)
  end;

  TCMYKColor = record
    Cyan: byte;       // range(0..100) percentage
    Magenta: byte;    // range(0..100)
    Yellow: byte;     // range(0..100)
    Black: byte;      // range(0..100)
  end;

{
  TColor converting functions
}
function TColorToRGBColor(const Color: TColor): TRGBColor;
function TColorToHSVColor(const Color: TColor): THSVColor;
procedure TColorToHSV(const color: TColor; out h, s, v: integer);

{
  RGBColor converting functions
}
function RGBColorToColor(const Color: TRGBColor): TColor;

{
  RGB variables converting functions
}
function RGBToRGBColor(const r, g, b: byte): TRGBColor;

{
  HSVColor converting functions
}
function HSVColorToColor(const hsvColor: THSVColor): TColor;
procedure HSVColorToRGB(const hsvColor: THSVColor; out R, G, B: byte);

{
  HSV variables converting functions
}
function HSVToHSVColor(const h, s, v: integer): THSVColor;
function HSVToColor(const h, s, v: integer): TColor;

{
  Converting RGB<->HSV functions
}
function HSVColorToRGBColor(const hsvColor: THSVColor): TRGBColor;
function RGBColorToHSVColor(const rgbColor: TRGBColor): THSVColor;

{
  Converting CMYK<->RGB functions
}
function CMYKColorToRGBColor(const cmykColor: TCMYKColor): TRGBColor;
function RGBColorToCMYKColor(const rgbColor: TRGBColor): TCMYKColor;
procedure CMYKColorToRGB(const cmykColor: TCMYKColor; out r, g, b: byte);

{
  Other functions
}
function EqualColors(const color1, color2: TRGBColor): boolean;
function ColorToStr(const rgbColor: TRGBColor; const format: string = 'HTML'; const capitalLetters: Boolean = false): string;
function StrToColor(ColorName: string; const format: string = 'HTML'): TRGBColor;
function InvertColor(const color: TColor): TColor;

function SafeWebColor(const rgbColor: TRGBColor): TRGBColor;
function IsSafeWebColor(const rgbColor: TRGBColor): Boolean;

implementation

function TColorToRGBColor(const Color: TColor): TRGBColor;
var
  r, g, b: byte;
begin
  r := Red(Color);
  g := Green(Color);
  b := Blue(Color);

  Result := RGBToRGBColor(r, g, b);
end { TColorToRGBColor };

function TColorToHSVColor(const Color: TColor): THSVColor;
begin
  Result := RGBColorToHSVColor(TColorToRGBColor(Color));
end { TColorToHSVColor };

procedure TColorToHSV(const color: TColor; out h, s, v: integer);
var
  hsvColor: THSVColor;
begin
  hsvColor := TColorToHSVColor(color);

  with hsvColor do
  begin
    h := hsvColor.Hue;
    s := hsvColor.Saturation;
    v := hsvColor.Value;
  end;
end { TColorToHSV };

function RGBColorToColor(const Color: TRGBColor): TColor;
begin
  Result := RGBToColor(Color.Red, Color.Green, Color.Blue);
end { RGBColorToColor};

function RGBToRGBColor(const r, g, b: byte): TRGBColor;
begin
  with Result do
  begin
    Red   := r;
    Green := g;
    Blue  := b;
  end;
end { RGBToRGBColor };

function HSVColorToColor(const hsvColor: THSVColor): TColor;
begin
  Result := RGBColorToColor(HSVColorToRGBColor(hsvColor));
end { HSVColorToColor };

procedure HSVColorToRGB(const hsvColor: THSVColor; out R, G, B: byte);
var
  rgbColor: TRGBColor;
begin
  rgbColor := HSVColorToRGBColor(hsvColor);

  R := rgbColor.Red;
  G := rgbColor.Green;
  B := rgbColor.Blue;
end { HSVColorToRGB };

function HSVToHSVColor(const h, s, v: integer): THSVColor;
begin
  with Result do
  begin
    Hue        := h;
    Saturation := s;
    Value      := v;
  end;
end { HSVToHSVColor };

function HSVToColor(const h, s, v: integer): TColor;
begin
  Result := HSVColorToColor(HSVToHSVColor(h, s, v));
end { HSVToColor };

function HSVColortoRGBColor(const hsvColor: THSVColor): TRGBColor;
var
  f: single;
  i: integer;
  hTemp: single;
  p, q, t: single;
  H, S, V: single;
  R, G, B: single;
begin
  {
    we want this kind of input:
    H: range(0..360.0) from range(0..360) or -1
    S: range(0..1.0)   from range(0..100)
    V: range(0..1.0)   from range(0..100)
  }
  with hsvColor do
  begin
    H := Hue;
    S := Saturation / 100.0;
    V := Value / 100.0;
  end;

  if V = 0.0 then
  begin
    R := 0;
    G := 0;
    B := 0;
  end
  else
  begin
    if H = 360.0 then // 360 degrees same as 0 degrees of course
      hTemp := 0.0
    else
      hTemp := H;

    hTemp := hTemp / 60; // h is now In [0,6)
    i := TRUNC(hTemp);   // largest integer <= h
    f := hTemp - i;      // fractional part of h

    p := V * (1.0 - S);
    q := V * (1.0 - (S * f));
    t := V * (1.0 - (S * (1.0 - f)));

    case i of
      0:
      begin
        R := V;
        G := t;
        B := p;
      end;
      1:
      begin
        R := q;
        G := V;
        B := p;
      end;
      2:
      begin
        R := p;
        G := V;
        B := t;
      end;
      3:
      begin
        R := p;
        G := q;
        B := V;
      end;
      4:
      begin
        R := t;
        G := p;
        B := V;
      end;
      5:
      begin
        R := V;
        G := p;
        B := q;
      end;
    end;
  end;

  {
    we want this kind of output:
    R: range(0..255)   from range(0..1.0)
    G: range(0..255)   from range(0..1.0)
    B: range(0..255)   from range(0..1.0)
  }
  R := R * 255.0;
  G := G * 255.0;
  B := B * 255.0;

  with Result do
  begin
    Red := Round(R);
    Green := Round(G);
    Blue := Round(B);
  end;
end { HSVColortoRGBColor };

function RGBColorToHSVColor(const rgbColor: TRGBColor): THSVColor;
var
  Delta: single;
  Minimum: single;
  H, S, V: single;
  R, G, B: single;
begin
  {
    we want this kind of input:
    R: range(0..1.0)
    G: range(0..1.0)
    B: range(0..1.0)
  }
  with rgbColor do
  begin
    R := Red / 255.0;
    G := Green / 255.0;
    B := Blue / 255.0;
  end;

  Minimum := MinValue([R, G, B]);
  V := MaxValue([R, G, B]);

  Delta := V - Minimum;

  // Calculate saturation:  saturation is 0 if r, g and b are all 0
  if V = 0.0 then
    S := 0
  else
    S := Delta / V;

  if S = 0.0 then // Achromatic:  When s = 0, h is undefined but assigned the value 0
    H := 0
  else            // Chromatic
  begin
    if R = V then     // degrees -- between yellow and magenta
      H := 60.0 * (G - B) / Delta
    else
    begin
      if G = V then   // between cyan and yellow
        H := 120.0 + 60.0 * (B - R) / Delta
      else
      begin
        if B = V then // between magenta and cyan
          H := 240.0 + 60.0 * (R - G) / Delta;
      end;
    end;

    if H < 0.0 then
      H := H + 360.0;
  end;

  {
    we want this kind of output:
    H: range(0..360) or -1
    S: range(0..100)
    V: range(0..100)
  }
  S := S * 100.0;
  V := V * 100.0;

  with Result do
  begin
    Hue := Round(H);
    Saturation := Round(S);
    Value := Round(V);
  end;
end { RGBColorToHSVColor };

function CMYKColorToRGBColor(const cmykColor: TCMYKColor): TRGBColor;
var
  R, G, B: single;
  C, M, Y, K: single;
begin
  with cmykColor do
  begin
    C := Cyan / 100.0;
    M := Magenta / 100.0;
    Y := Yellow / 100.0;
    K := Black / 100.0;
  end;

  R := C * (1.0 - K) + K;
  G := M * (1.0 - K) + K;
  B := Y * (1.0 - K) + K;

  R := (1.0 - R) * 255.0;
  G := (1.0 - G) * 255.0;
  B := (1.0 - B) * 255.0;

  with Result do
  begin
    Red := Round(R);
    Green := Round(G);
    Blue := Round(B);
  end;
end {CMYtoRGBTriple};

procedure CMYKColorToRGB(const cmykColor: TCMYKColor; out r, g, b: byte);
var
  tempRGBColor: TRGBColor;
begin
  tempRGBColor := CMYKColorToRGBColor(cmykColor);
  r := tempRGBColor.Red;
  g := tempRGBColor.Green;
  b := tempRGBColor.Blue;
end;

// R, G, B, C, M, Y each IN [0..255]
function RGBColorToCMYKColor(const rgbColor: TRGBColor): TCMYKColor;
var
  R, G, B: single;
  C, M, Y, K: single;
begin
  with rgbColor do
  begin
    R := 1.0 - (Red / 255.0);
    G := 1.0 - (Green / 255.0);
    B := 1.0 - (Blue / 255.0);
  end;

  if (R < G) then
    K := R
  else
    K := G;
  if (B < K) then
    K := B;

  if (K <> 1.0) then
  begin
    C := (R - K) / (1.0 - K);
    M := (G - K) / (1.0 - K);
    Y := (B - K) / (1.0 - K);
  end
  else
  begin
    C := 0;
    M := 0;
    Y := 0;
  end;

  C := (C * 100);
  M := (M * 100);
  Y := (Y * 100);
  K := (K * 100);

  with Result do
  begin
    Cyan := Round(C);
    Magenta := Round(M);
    Yellow := Round(Y);
    Black := Round(K);
  end;
end {RGBtoCMYK};

function EqualColors(const color1, color2: TRGBColor): boolean;
begin
  if ((color1.Red = color2.Red) and (color1.Green = color2.Green) and
    (color1.Blue = color2.Blue)) then
    Result := True
  else
    Result := False;
end { equalColors };

function ColorToStr(const rgbColor: TRGBColor; const format: string = 'HTML'; const capitalLetters: Boolean = false): string;
var
  cmykColor: TCMYKColor;
  hsvColor: THSVColor;
begin
  with rgbColor do
  begin
    if (format = 'HTML') then
    begin
      Result := '#' + IntToHex(Red, 2) + IntToHex(Green, 2) + IntToHex(Blue, 2);
    end
    else if (format = 'RGB') then
    begin
      Result := 'rgb(' + IntToStr(Red) + ', ' + IntToStr(Green) + ', ' + IntToStr(Blue) + ')';
    end
    else if (format = 'Pascal') then
    begin
      Result := '$00' + IntToHex(Blue, 2) + IntToHex(Green, 2) + IntToHex(Red, 2);
    end
    else if (format = 'C++') then
    begin
      Result := '0x00' + IntToHex(Blue, 2) + IntToHex(Green, 2) + IntToHex(Red, 2);
    end
    else if (format = 'VB') then
    begin
      Result := '&H00' + IntToHex(Blue, 2) + IntToHex(Green, 2) + IntToHex(Red, 2) + '&';
    end
    else if (format = 'Java') then
    begin
      Result := '0x' + IntToHex(Red, 2) + IntToHex(Green, 2) + IntToHex(Blue, 2);
    end
    else if (format = 'Hex') then
    begin
      Result := IntToHex(Red, 2) + IntToHex(Green, 2) + IntToHex(Blue, 2);
    end
    else if (format = 'CMYK') then
    begin
      cmykColor := RGBColorToCMYKColor(rgbColor);
      Result := '(' + IntToStr(cmykColor.Cyan) + ', ' +
        IntToStr(cmykColor.Magenta) + ', ' + IntToStr(cmykColor.Yellow) +
        ', ' + IntToStr(cmykColor.Black) + ')';
    end
    else if (format = 'HSV') then
    begin
      hsvColor := RGBColorToHSVColor(rgbColor);
      Result := '(' + IntToStr(hsvColor.Hue) + ', ' +
        IntToStr(hsvColor.Saturation) + ', ' + IntToStr(hsvColor.Value) + ')';
    end;
  end;

  if (capitalLetters = True) then
    Result := AnsiUpperCase(Result)
  else
    Result := AnsiLowerCase(Result);
end;

function StrToColor(ColorName: string; const format: string = 'HTML'): TRGBColor;
const
  ERROR_TITLE = 'Błąd';
  ERROR_MSG = 'Wpisany kod koloru jest nieprawidłowy.';
var
  cmykColor: TCMYKColor;
  hsvColor: THSVColor;
  tmpStrList: TStringList;
  R, G, B: integer;
begin
  ColorName := AnsiLowerCase(Trim(ColorName));

  with Result do
  begin
    try
      if (ColorName <> '') then
      begin
        if (format = 'HTML') then
        begin
          //Result := AnsiLowerCase('#'+IntToHex(r,2)+IntToHex(g,2)+IntToHex(b,2))
          //if not (pos('#', ColorName) = 1) then
          if not (ExecRegExpr('^#([0-9a-fA-F]{3}){1,2}$', ColorName)) then
            raise Exception.Create(ERROR_MSG);

          Delete(ColorName, 1, 1);

          if (Length(ColorName) = 6) then
          begin
            R := StrToInt('$' + Copy(ColorName, 1, 2));
            G := StrToInt('$' + Copy(ColorName, 3, 2));
            B := StrToInt('$' + Copy(ColorName, 5, 2));
          end
          else if (Length(ColorName) = 3) then
          begin
            R := StrToInt('$' + DupeString(Copy(ColorName, 1, 1), 2));
            G := StrToInt('$' + DupeString(Copy(ColorName, 2, 1), 2));
            B := StrToInt('$' + DupeString(Copy(ColorName, 3, 1), 2));
          end
          else
            raise Exception.Create(ERROR_MSG);

          if (((R >= 0) and (R <= 255)) and
            ((G >= 0) and (G <= 255)) and ((B >= 0) and (B <= 255))) then
          begin
            with Result do
            begin
              Red := R;
              Green := G;
              Blue := B;
            end;
          end
          else
            raise Exception.Create(ERROR_MSG);
        end
        else if (format = 'RGB') then
        begin
          //Result := AnsiLowerCase('rgb('+IntToStr(r)+', '+IntToStr(g)+', '+IntToStr(b)+')')
          if not (ExecRegExpr(
            '^rgb\(\s*[0-9]{1,3}\s*\,\s*[0-9]{1,3}\s*\,\s*[0-9]{1,3}\s*\)$', ColorName)) then
            raise Exception.Create(ERROR_MSG);

          Delete(ColorName, 1, 4);
          Delete(ColorName, Length(ColorName), 1);

          try
            tmpStrList := TStringList.Create;
            ExtractStrings([','], [], PChar(ColorName), tmpStrList);

            R := StrToInt(Trim(tmpStrList.Strings[0]));
            G := StrToInt(Trim(tmpStrList.Strings[1]));
            B := StrToInt(Trim(tmpStrList.Strings[2]));
          finally
            tmpStrList.Free;
          end;

          if (((R >= 0) and (R <= 255)) and
            ((G >= 0) and (G <= 255)) and ((B >= 0) and (B <= 255))) then
          begin
            with Result do
            begin
              Red := R;
              Green := G;
              Blue := B;
            end;
          end
          else
            raise Exception.Create(ERROR_MSG);
        end
        else if (format = 'Pascal') then
        begin
          //Result := AnsiLowerCase('$00'+IntToHex(b,2)+IntToHex(g,2)+IntToHex(r,2))
          if not (ExecRegExpr('^\$00[0-9a-fA-F]{6}$', ColorName)) then
            raise Exception.Create(ERROR_MSG);

          Delete(ColorName, 1, 3);

          R := StrToInt('$' + Copy(ColorName, 5, 2));
          G := StrToInt('$' + Copy(ColorName, 3, 2));
          B := StrToInt('$' + Copy(ColorName, 1, 2));

          if (((R >= 0) and (R <= 255)) and
            ((G >= 0) and (G <= 255)) and ((B >= 0) and (B <= 255))) then
          begin
            with Result do
            begin
              Red := R;
              Green := G;
              Blue := B;
            end;
          end
          else
            raise Exception.Create(ERROR_MSG);
        end
        else if (format = 'C++') then
        begin
          //Result := AnsiLowerCase('0x00'+IntToHex(b,2)+IntToHex(g,2)+IntToHex(r,2))
          if not (ExecRegExpr('^0x00[0-9a-fA-F]{6}$', ColorName)) then
            raise Exception.Create(ERROR_MSG);

          Delete(ColorName, 1, 4);

          R := StrToInt('$' + Copy(ColorName, 5, 2));
          G := StrToInt('$' + Copy(ColorName, 3, 2));
          B := StrToInt('$' + Copy(ColorName, 1, 2));

          if (((R >= 0) and (R <= 255)) and
            ((G >= 0) and (G <= 255)) and ((B >= 0) and (B <= 255))) then
          begin
            with Result do
            begin
              Red := R;
              Green := G;
              Blue := B;
            end;
          end
          else
            raise Exception.Create(ERROR_MSG);
        end
        else if (format = 'VB') then
        begin
          //Result := AnsiLowerCase('&H00'+IntToHex(b,2)+IntToHex(g,2)+IntToHex(r,2)+'&')
          if not (ExecRegExpr('^&H00[0-9a-fA-F]{6}&$', ColorName)) then
            raise Exception.Create(ERROR_MSG);

          Delete(ColorName, 1, 4);
          Delete(ColorName, Length(ColorName), 1);

          R := StrToInt('$' + Copy(ColorName, 5, 2));
          G := StrToInt('$' + Copy(ColorName, 3, 2));
          B := StrToInt('$' + Copy(ColorName, 1, 2));

          if (((R >= 0) and (R <= 255)) and
            ((G >= 0) and (G <= 255)) and ((B >= 0) and (B <= 255))) then
          begin
            with Result do
            begin
              Red := R;
              Green := G;
              Blue := B;
            end;
          end
          else
            raise Exception.Create(ERROR_MSG);
        end
        else if (format = 'Java') then
        begin
          //Result := AnsiLowerCase('0x'+IntToHex(r,2)+IntToHex(g,2)+IntToHex(b,2))
          if not (ExecRegExpr('^0x[0-9a-fA-F]{6}$', ColorName)) then
            raise Exception.Create(ERROR_MSG);

          Delete(ColorName, 1, 2);

          R := StrToInt('$' + Copy(ColorName, 1, 2));
          G := StrToInt('$' + Copy(ColorName, 3, 2));
          B := StrToInt('$' + Copy(ColorName, 5, 2));

          if (((R >= 0) and (R <= 255)) and
            ((G >= 0) and (G <= 255)) and ((B >= 0) and (B <= 255))) then
          begin
            with Result do
            begin
              Red := R;
              Green := G;
              Blue := B;
            end;
          end
          else
            raise Exception.Create(ERROR_MSG);
        end
        else if (format = 'Hex') then
        begin
          //Result := AnsiLowerCase(IntToHex(r,2)+IntToHex(g,2)+IntToHex(b,2))
          if not (ExecRegExpr('^([0-9a-fA-F]{3}){1,2}$', ColorName)) then
            raise Exception.Create(ERROR_MSG);

          if (Length(ColorName) = 6) then
          begin
            R := StrToInt('$' + Copy(ColorName, 1, 2));
            G := StrToInt('$' + Copy(ColorName, 3, 2));
            B := StrToInt('$' + Copy(ColorName, 5, 2));
          end
          else if (Length(ColorName) = 3) then
          begin
            R := StrToInt('$' + DupeString(Copy(ColorName, 1, 1), 2));
            G := StrToInt('$' + DupeString(Copy(ColorName, 2, 1), 2));
            B := StrToInt('$' + DupeString(Copy(ColorName, 3, 1), 2));
          end
          else
            raise Exception.Create(ERROR_MSG);

          if (((R >= 0) and (R <= 255)) and
            ((G >= 0) and (G <= 255)) and ((B >= 0) and (B <= 255))) then
          begin
            with Result do
            begin
              Red := R;
              Green := G;
              Blue := B;
            end;
          end
          else
            raise Exception.Create(ERROR_MSG);
        end
        else if (format = 'CMYK') then
        begin
          if not (ExecRegExpr(
            '^\(\s*[0-9]{1,3}\s*\,\s*[0-9]{1,3}\s*\,\s*[0-9]{1,3}\s*\,\s*[0-9]{1,3}\s*\)$',
            ColorName)) then
            raise Exception.Create(ERROR_MSG);

          Delete(ColorName, 1, 1);
          Delete(ColorName, Length(ColorName), 1);

          try
            tmpStrList := TStringList.Create;
            ExtractStrings([','], [], PChar(ColorName), tmpStrList);

            cmykColor.Cyan := StrToInt(Trim(tmpStrList.Strings[0]));
            cmykColor.Magenta := StrToInt(Trim(tmpStrList.Strings[1]));
            cmykColor.Yellow := StrToInt(Trim(tmpStrList.Strings[2]));
            cmykColor.Black := StrToInt(Trim(tmpStrList.Strings[3]));
          finally
            tmpStrList.Free;
          end;

          if (((cmykColor.Cyan >= 0) and (cmykColor.Cyan <= 100)) and
            ((cmykColor.Magenta >= 0) and (cmykColor.Magenta <= 100)) and
            ((cmykColor.Yellow >= 0) and (cmykColor.Yellow <= 100)) and
            ((cmykColor.Black >= 0) and (cmykColor.Black <= 100))) then
          begin
            with Result do
            begin
              CMYKColorToRGB(cmykColor, Red, Green, Blue);
            end;
          end
          else
            raise Exception.Create(ERROR_MSG);
        end
        else if (format = 'HSV') then
        begin
          if not (ExecRegExpr(
            '^\(\s*[0-9]{1,3}\s*\,\s*[0-9]{1,3}\s*\,\s*[0-9]{1,3}\s*\)$', ColorName)) then
            raise Exception.Create(ERROR_MSG);

          Delete(ColorName, 1, 1);
          Delete(ColorName, Length(ColorName), 1);

          try
            tmpStrList := TStringList.Create;
            ExtractStrings([','], [], PChar(ColorName), tmpStrList);

            hsvColor.Hue := StrToInt(Trim(tmpStrList.Strings[0]));
            hsvColor.Saturation := StrToInt(Trim(tmpStrList.Strings[1]));
            hsvColor.Value := StrToInt(Trim(tmpStrList.Strings[2]));
          finally
            tmpStrList.Free;
          end;

          if (((hsvColor.Hue >= 0) and (hsvColor.Hue <= 360)) and
            ((hsvColor.Saturation >= 0) and (hsvColor.Saturation <= 100)) and
            ((hsvColor.Value >= 0) and (hsvColor.Value <= 100))) then
          begin
            with Result do
            begin
              HSVColorToRGB(hsvColor, Red, Green, Blue);
            end;
          end
          else
            raise Exception.Create(ERROR_MSG);
        end;
      end;
    except
      Red := 255;
      Green := 255;
      Blue := 255;

      Application.MessageBox(ERROR_MSG, ERROR_TITLE, MB_ICONERROR);
    end;
  end;
end;

function InvertColor(const color: TColor): TColor;
var
  grayLevel: integer;
begin
  grayLevel := (77 * (color and $FF) + 151 * (color shr 8 and $FF) + 28 *
    (color shr 16 and $FF)) shr 8;
  if grayLevel < 128 then
    Result := clWhite
  else
    Result := clBlack;
end;

function SafeWebColor(const rgbColor: TRGBColor): TRGBColor;
var
  rgbColorTmp: TRGBColor;
begin
  // allow values: (0, 51, 102, 153, 204, 255)
  //               00, 33, 66,  99,  cc,  ff

  rgbColorTmp.Red   := Round(rgbColor.Red / 51) * 51;
  rgbColorTmp.Green := Round(rgbColor.Green / 51) * 51;
  rgbColorTmp.Blue  := Round(rgbColor.Blue / 51) * 51;

  Result := rgbColorTmp;
end;

function IsSafeWebColor(const rgbColor: TRGBColor): Boolean;
var
  rgbColorTmp: TRGBColor;
begin
  rgbColorTmp := SafeWebColor(rgbColor);

  if ((rgbColorTmp.Red = rgbColor.Red)
       AND (rgbColorTmp.Green = rgbColor.Green)
       AND (rgbColorTmp.Blue = rgbColor.Blue)) then
    Result := True
  else
    Result := False;
end;

end.

