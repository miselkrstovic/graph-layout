unit Utils;

interface

uses Classes, MMSystem, Windows, SysUtils;

const
  {$IFDEF UNIX}
    DirDelimiter = '/';
    DirSeparator = ':';
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}
    DirDelimiter = '\';
    DirSeparator = ';';
  {$ENDIF MSWINDOWS}

procedure PlayWave(soundName: String);
function Occurrences(const Substring, Text: string): integer;
function Element(const elementNumber: integer; const delimiter: String; const source: String): String;
function Explode(const separator: Char; const str: String; makelowercase: boolean = false): TStringList; Overload;
function Explode(const separator: Char; const str: String; limit: Integer; makelowercase: boolean = false): TStringList; Overload;

implementation

procedure PlayWave(soundName: String);
var
    Filename : String;
begin
    // SND_SYNC specifies that the sound is played synchronously and the function does not return until the sound ends.
    // SND_ASYNC specifies that the sound is played asynchronously and the function returns immediately after beginning the sound.
    // SND_NODEFAULT specifies that if the sound cannot be found, the function returns silently without playing the default sound.
    // SND_LOOP specifies that the sound will continue to play continuously until sndPlaySound is called again with the lpszSoundName$ parameter set to null. You must also specify the SND_ASYNC flag to loop sounds.
    // SND_NOSTOP specifies that if a sound is currently playing, the function will immediately return False without playing the requested sound.
    try
        Filename := 'sounds'+DirDelimiter+soundName+'.wav';
        PlaySound(PChar(Filename), 0, SND_ASYNC);
    except
        Beep;
    end;
end;

function Element(const elementNumber: integer; const delimiter: String; const source: String): String;
var
  i    : Integer;
  head : Integer;
  s    :  String;
begin
  result := '';
  s := source + delimiter; {Stupid patch}
  for i := 0 to elementNumber do begin
    head := Pos(delimiter, s);
    If head>0 then result := copy (s, 1, head-1)
              else result := s;
    Delete(s, 1, head);
  end;
end;

function Explode(const separator: Char; const str: String; makelowercase: boolean = false): TStringList;
begin
  result := Explode(separator, str, 0);
end;

function Explode(const separator: Char; const str: String; limit: Integer; makelowercase : boolean = false): TStringList;
var
  count : Integer;
  i     : Integer;
  temp  : TStringList;
  Token : String;
begin
  temp := TStringList.Create;
  try
    count := Occurrences(separator, str);

    for I := 0 to count do begin
      Token := trim(Element(i, separator, str));
      If Length(Token)<>0 then begin
        if makelowercase then begin
          temp.Add(LowerCase(Token));
        end else begin
          temp.Add(Token);
        end;
      end;
    end;
  finally
    result := temp;
  end;
end;

function Occurrences(const Substring, Text: string): integer;
var
  i : Integer;
begin
  result := 0;
  for i := 1 to Length(Text) do
      if Text[i]=Substring then Inc(result);
end;

end.
