(*
 * @(#)Graph.java	1.18 06/02/22
 *
 * Copyright (c) 2006 Sun Microsystems, Inc. All Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * -Redistribution of source code must retain the above copyright notice, this
 *  list of conditions and the following disclaimer.
 *
 * -Redistribution in binary form must reproduce the above copyright notice,
 *  this list of conditions and the following disclaimer in the documentation
 *  and/or other materials provided with the distribution.
 *
 * Neither the name of Sun Microsystems, Inc. or the names of contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 *
 * This software is provided "AS IS," without a warranty of any kind. ALL
 * EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND WARRANTIES, INCLUDING
 * ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE
 * OR NON-INFRINGEMENT, ARE HEREBY EXCLUDED. SUN MICROSYSTEMS, INC. ("SUN")
 * AND ITS LICENSORS SHALL NOT BE LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE
 * AS A RESULT OF USING, MODIFYING OR DISTRIBUTING THIS SOFTWARE OR ITS
 * DERIVATIVES. IN NO EVENT WILL SUN OR ITS LICENSORS BE LIABLE FOR ANY LOST
 * REVENUE, PROFIT OR DATA, OR FOR DIRECT, INDIRECT, SPECIAL, CONSEQUENTIAL,
 * INCIDENTAL OR PUNITIVE DAMAGES, HOWEVER CAUSED AND REGARDLESS OF THE THEORY
 * OF LIABILITY, ARISING OUT OF THE USE OF OR INABILITY TO USE THIS SOFTWARE,
 * EVEN IF SUN HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 *
 * You acknowledge that this software is not designed, licensed or intended
 * for use in the design, construction, operation or maintenance of any
 * nuclear facility.
 *)

(*
 * @(#)Graph.java	1.18 06/02/22
 *)

unit Graph;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, Utils, Math, ExtCtrls;

const
  fixedColor = clRed;
  selectColor = $008080FF;
  edgeColor = clBlack;
  nodeColor = $0064DCFA;
  stressColor = clGray;
  arcColor1 = clBlack;
  arcColor2 = clPurple;
  arcColor3 = clRed;

type
  TMultiArray = array of array of String;

  TNode = class
    x : double;
    y : double;
    dx : double;
    dy : double;
    fixed : boolean;
    lbl : String;
  end;

  TEdge = class
    from : integer;
    to_   : integer;
    len  : double;
  end;

  TFontMetrics = class
  private
    ownerCanvas : TCanvas;
  public
    constructor Create(ACanvas : TCanvas);
    function stringWidth(Value : String): integer;
    function getHeight: integer;
    function getAscent(): integer;
  end;

  TfrmGraph = class;

  TDimension = record
    width : integer;
    height : integer;
  end;

  TGraphPanel = class(TPaintBox)
  private
    bmousedown : boolean;
  public
    internaltimer : TTimer;

    graph : TfrmGraph;
    nnodes : integer;
    nodes : array [0..99] of TNode;

    nedges : integer;
    edges : array [0..199] of TEdge;

    relaxer : TThread;
    bStress : boolean;
    bRandom : boolean;

    numMouseButtonsDown : integer;

    pick : TNode;
    pickfixed : boolean;
    offscreen : TImage;
    offscreensize : TDimension;
    offgraphics : TCanvas;

    constructor Create(graph : TfrmGraph);
    destructor Destroy; override;
    function getSize(): TDimension;
    function findNode(lbl : String): integer;
    function addNode(lbl : String): integer;
    procedure addEdge(from : String; to_ : String; len : integer);
    procedure run(Sender : TObject);
    procedure relax();
    procedure paintNode(g : TCanvas; n : TNode; fm : TFontMetrics);
    function getBackground(): TColor;
    procedure update(g : TCanvas);

    // Event handlers
    procedure mousePressed(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);

    procedure mouseReleased(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);

    procedure mouseDragged(Sender: TObject; Shift: TShiftState;
    X, Y: Integer);

    procedure start();

    procedure stop();

    procedure Paint(); override;

    function createImage(width, height : integer): TImage;
  end;

  TfrmGraph = class(TForm)
    controlPanel: TPanel;
    btnScramble: TButton;
    btnShake: TButton;
    chkStress: TCheckBox;
    chkRandom: TCheckBox;
    edgesPanel: TPanel;
    edtCenter: TEdit;
    edtEdges: TComboBox;
    lblEdges: TLabel;
    lblCenter: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkStressClick(Sender: TObject);
    procedure btnScrambleClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edtEdgesChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    panel : TGraphPanel;
    procedure init();
    function getSize(): TDimension;
  published
    function getAppletInfo(): String;
    function getParameterInfo(): TMultiArray;
  end;

var
  frmGraph: TfrmGraph;

implementation

{$R *.dfm}

{ TGraphPanel }

procedure TGraphPanel.Paint();
begin
  update(Self.Canvas);
  inherited;
end;

constructor TGraphPanel.Create(graph : TfrmGraph);
begin
  inherited Create(graph);

  Self.graph := graph;

  numMouseButtonsDown := 0;

  // Assign mouse listeners
  Self.OnMouseDown := mousePressed;
  Self.OnMouseUp   := mouseReleased;
  Self.OnMouseMove := mouseDragged;
end;

function TGraphPanel.createImage(width, height: integer): TImage;
begin
  result := TImage.Create(Self);
  Result.Width := width;
  Result.Height := height;
end;

destructor TGraphPanel.Destroy;
var
  i: Integer;
begin
  for i := High(nodes) downto Low(nodes) do FreeAndNil(nodes[i]);
  for i := High(edges) downto Low(edges) do FreeAndNil(edges[i]);

  inherited;
end;

function TGraphPanel.getBackground: TColor;
begin
  result := Self.Color;
end;

function TGraphPanel.getSize(): TDimension;
begin
  result.Width := Self.Width;
  result.Height := Self.Height;
end;

function TGraphPanel.findNode(lbl : String): integer;
var
  i : integer;
begin
  for i := 0 to nnodes-1 do begin
    if (nodes[i].lbl = lbl) then begin
      result := i;
      exit;
    end;
  end;
  result := addNode(lbl);
end;

function TGraphPanel.addNode(lbl : String): integer;
var
  n : TNode;
begin
  n := TNode.Create();
  n.x := 10 + 380*random();
  n.y := 10 + 380*random();
  n.lbl := lbl;
  nodes[nnodes] := n;
  result := nnodes;
  inc(nnodes);
end;

procedure TGraphPanel.addEdge(from : String; to_ : String; len : integer);
var
  e : TEdge;
begin
	e := TEdge.Create();
	e.from := findNode(from);
	e.to_ := findNode(to_);
	e.len := len;                                 
	edges[nedges] := e;
  inc(nedges);
end;

procedure TGraphPanel.run(Sender : TObject);
var
  n : TNode;
begin
  internaltimer.Enabled := false;
  try
// TODO: Remove these comments
//  Thread me = Thread.currentThread();
//	while (relaxer == me) do begin
	    relax();
	    if (bRandom and (random() < 0.03)) then begin
		    n := nodes[trunc(random() * nnodes)];
        if not(n.fixed) then begin
          n.x := n.x + 100*random() - 50;
          n.y := n.y + 100*random() - 50;
        end;
        PlayWave('drip');
	    end;
//	    try {
//		Thread.sleep(100);
//	    } catch (InterruptedException e) {
//		break;
//	    }
//	end;
  finally
    internaltimer.Enabled := true;
  end;
end;

procedure TGraphPanel.paintNode(g : TCanvas; n : TNode; fm : TFontMetrics);
var
  x : integer;
  y : integer;
  w : integer;
  h : integer;
begin
  x := trunc(n.x);
  y := trunc(n.y);

  if n = pick then begin
    g.Brush.Color := selectColor;
  end else begin
    if n.fixed then begin
      g.Brush.Color := fixedColor;
    end else begin
      g.Brush.Color := nodeColor;
    end;
  end;

  w := fm.stringWidth(n.lbl) + 10;
  h := fm.getHeight() + 4;
  g.FillRect(Rect(x - w div 2, y - h div 2, w+x - w div 2, h+y - h div 2));
  g.Pen.Color := clBlack;
  g.Rectangle(x - w div 2, y - h div 2, w-1 + x - w div 2, h-1 + y - h div 2);
  g.TextOut(x - (w-10) div 2, (y - (h-4) div 2) + fm.getAscent(), n.lbl);
end;

procedure TGraphPanel.update(g : TCanvas);
var
  d : TDimension;
  i : integer;
  e : TEdge;
  x1,
  y1,
  x2,
  y2,
  len : integer;
  lbl : String;
  fm  : TFontMetrics;
begin
  d := getSize();
  if ((offscreen = nil) or (d.width <> offscreensize.width)
    or (d.height <> offscreensize.height)) then begin
    offscreen := createImage(d.width, d.height);
    offscreensize := d;
    if (offgraphics <> nil) then begin
      offgraphics.free;
    end;

    offgraphics := offscreen.Canvas;
    offgraphics.Font.Assign(Self.Font);
  end;

  offgraphics.Brush.Color := getBackground();
  offgraphics.FillRect(Rect(0, 0, d.width, d.height));
  for i := 0 to nedges-1 do begin
    e := edges[i];
    x1 := trunc(nodes[e.from].x);
    y1 := trunc(nodes[e.from].y);
    x2 := trunc(nodes[e.to_].x);
    y2 := trunc(nodes[e.to_].y);
    len := trunc(abs(sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)) - e.len));
    if (len < 10) then begin
      offgraphics.Pen.Color := arcColor1;
    end else begin
      if (len < 20) then begin
        offgraphics.Pen.Color := arcColor2;
      end else begin
        offgraphics.Pen.Color := arcColor3;
      end;
    end;
    offgraphics.MoveTo(x1, y1);
    offgraphics.LineTo(x2, y2);
    if (bStress) then begin
      lbl := IntToStr(len);
      offgraphics.Pen.Color:= stressColor;
      offgraphics.TextOut(x1 + (x2-x1) div 2, y1 + (y2-y1) div 2, lbl);
      offgraphics.Pen.Color := edgeColor;
    end;
  end;

  fm := TFontMetrics.Create(Self.Canvas);
  try
    for i := 0 to nnodes -1 do begin
      paintNode(offgraphics, nodes[i], fm);
    end;
    g.Draw(0, 0, offscreen.Picture.Graphic);
  finally
    fm.Free;
  end;
end;

procedure TGraphPanel.relax();
var
  i,
  j : integer;
  e : TEdge;
  vx,
  vy,
  len,
  f,
  dx,
  dy : double;
  n1,
  n2,
  n : TNode;
  dlen : double;
  d : TDimension;
begin
	for i := 0 to nedges-1 do begin
	    e := edges[i];
	    vx := nodes[e.to_].x - nodes[e.from].x;
	    vy := nodes[e.to_].y - nodes[e.from].y;
	    len := sqrt(vx * vx + vy * vy);
      if len = 0 then begin
        len := 0.0001;
      end else begin
        len := len;
      end;
	    f := (edges[i].len - len) / (len * 3);
	    dx := f * vx;
	    dy := f * vy;

	    nodes[e.to_].dx := nodes[e.to_].dx + dx;
	    nodes[e.to_].dy := nodes[e.to_].dy + dy;
	    nodes[e.from].dx := nodes[e.from].dx -dx;
	    nodes[e.from].dy := nodes[e.from].dy -dy;
	end;

	for i := 0 to nnodes-1 do begin
	    n1 := nodes[i];
	    dx := 0;
	    dy := 0;

	    for j := 0 to nnodes -1 do begin
        if (i = j) then begin
            continue;
        end;
        n2 := nodes[j];
        vx := n1.x - n2.x;
        vy := n1.y - n2.y;
        len := vx * vx + vy * vy;
        if (len = 0) then begin
            dx := dx + random();
            dy := dy + random();
        end else if (len < 100*100) then begin
            dx := dx + vx / len;
            dy := dy + vy / len;
        end;
      end;
	    dlen := dx * dx + dy * dy;
	    if (dlen > 0) then begin
        dlen := sqrt(dlen) / 2;
        n1.dx := n1.dx + dx / dlen;
        n1.dy := n1.dy + dy / dlen;
	    end;
	end;

	d := getSize();
	for i := 0 to nnodes-1 do begin
    n := nodes[i];
    if not(n.fixed) then begin
      n.x := n.x + max(-5, min(5, n.dx));
      n.y := n.y + max(-5, min(5, n.dy));
    end;
    if (n.x < 0) then begin
        n.x := 0;
    end else if (n.x > d.width) then begin
        n.x := d.width;
    end;
    if (n.y < 0) then begin
        n.y := 0;
    end else if (n.y > d.height) then begin
        n.y := d.height;
    end;
    n.dx := n.dx / 2;
    n.dy := n.dy / 2;
	end;
	Invalidate();
end;

procedure TGraphPanel.mouseReleased(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
  bmousedown := false;

  dec(numMouseButtonsDown);

  if Assigned(pick) then begin
    pick.fixed := pickfixed;
    pick.x := X;
    pick.y := Y;
  end;
  if (numMouseButtonsDown = 0) then begin
      pick := nil;
  end;

  Invalidate();
end;

procedure TGraphPanel.mousePressed(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
  bestdist : double;
  i : integer;
  n : TNode;
  dist : double;
begin
  bmousedown := true;

  inc(numMouseButtonsDown);
  bestdist := MaxDouble;

  for i := 0 to nnodes -1 do begin
    n := nodes[i];
    dist := (n.x - x) * (n.x - x) + (n.y - y) * (n.y - y);
    if (dist < bestdist) then begin
      pick := n;
      bestdist := dist;
    end;
  end;

  if Assigned(pick) then begin
    pickfixed := pick.fixed;
    pick.fixed := true;
    pick.x := x;
    pick.y := y;
  end;

  Invalidate();
end;

procedure TGraphPanel.mouseDragged(Sender: TObject; Shift: TShiftState;
    X, Y: Integer);
begin
  if bmousedown = true then begin
    if pick<>nil then begin
      pick.x := X;
      pick.y := Y;
      Invalidate();
    end;
  end;
end;

procedure TGraphPanel.start();
begin
  internaltimer := TTimer.Create(Self);
  internaltimer.OnTimer := run;
  internaltimer.Interval := 1;
  internaltimer.Enabled := true;
end;

procedure TGraphPanel.stop();
begin
  internaltimer.Free;
end;

{ TfrmGraph }

procedure TfrmGraph.FormShow(Sender: TObject);
begin
  DoubleBuffered := true;
  init();
end;

function TfrmGraph.getSize: TDimension;
begin
  result.Width := Self.Width;
  result.Height := Self.Height;
end;

procedure TfrmGraph.init();
var
  d : TDimension;
  n : TNode;
  len, i, j, x : integer;
  str : String;
  t : TStringList;
  fixedNodes : TStringList;
  tempNode : String;
begin
	panel := TGraphPanel.Create(Self);
  panel.Parent := Self;
	panel.Align := alClient;

  t := Explode(Char(','), edtEdges.Text);
  try
    for x := 0 to t.Count-1 do begin
      str := t.Strings[x];
      i := pos('-', str) -1;
      if (i > 0) then begin
        len := 50;
        j := pos('/', str)-1;
        if (j > 0) and (j<length(str)-1) then begin
            len := StrToInt(copy(str, j+2, length(str)-j));
            str := copy(str, 0, j);
        end;
        panel.addEdge(copy(str,0,i), copy(str, i+2, length(str)-i), len);
      end;
    end;
  finally
    FreeAndNil(t);
  end;

	d := getSize();
	if (edtCenter.Text <> '') then begin
      fixedNodes := Explode(Char(','), edtCenter.Text);
      try
        for i := 0 to fixedNodes.Count - 1 do begin
          tempNode := fixedNodes.Strings[i];
          n := panel.nodes[panel.findNode(tempNode)];
          n.x := d.width / 2;
          n.y := d.height / 2;
          n.fixed := true;
        end;
      finally
        fixedNodes.Free;
      end;
	end;

  panel.start;
end;

procedure TfrmGraph.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  panel.stop;
end;

procedure TfrmGraph.btnScrambleClick(Sender: TObject);
var
  d : TDimension;
  i : integer;
  n : TNode;
begin
	if (Sender = btnScramble) then begin
      PlayWave('computer');
	    d := getSize();
	    for i := 0 to panel.nnodes-1 do begin
        n := panel.nodes[i];
        if not(n.fixed) then begin
            n.x := 10 + (d.width-20)*random();
            n.y := 10 + (d.height-20)*random();
        end;
	    end;
    exit;
	end;

	if (Sender = btnShake) then begin
      PlayWave('gong');
	    d := getSize();
	    for i := 0 to panel.nnodes-1 do begin
        n := panel.nodes[i];
        if not(n.fixed) then begin
            n.x := n.x + 80*random() - 40;
            n.y := n.y + 80*random() - 40;
        end;
      end;
	end;
end;

procedure TfrmGraph.chkStressClick(Sender: TObject);
var
  on_ : boolean;
begin
  on_ := TCheckBox(Sender).Checked;
  if (Sender = chkStress) then panel.bStress := on_
  else if (Sender = chkRandom) then panel.bRandom := on_;
end;

procedure TfrmGraph.edtEdgesChange(Sender: TObject);
begin
  init()
end;

function TfrmGraph.getAppletInfo(): String;
begin
	result := 'Title: GraphLayout \nAuthor: <unknown>';
end;

function TfrmGraph.getParameterInfo(): TMultiArray;
var
  info: TMultiArray;
begin
  SetLength(info, 2);

  SetLength(info[0], 3);
  SetLength(info[1], 3);

	info[0] := ['edges', 'delimited string',
    'A comma-delimited list of all the edges.  It takes the form of ''C-N1,C-N2,'+
    'C-N3,C-NX,N1-N2/M12,N2-N3/M23,N3-NX/M3X,...'' where C is the name of center '+
    'node (see ''center'' parameter) and NX is a node attached to the center node.  '+
    'For the edges connecting nodes to each other (and not to the center node) '+
    'you may (optionally) specify a length MXY separated from the edge name by a '+
    'forward slash.'];
	info[1] := ['center', 'string', 'The name of the center node.'];
	result := info;
end;

procedure TfrmGraph.FormDestroy(Sender: TObject);
begin
  panel.Free;
  controlPanel.Free;
end;

{ TFontMetrics }

constructor TFontMetrics.Create(ACanvas: TCanvas);
begin
  ownercanvas := ACanvas;
end;
function TFontMetrics.getAscent: integer;
begin
  result := 0;
end;

function TFontMetrics.getHeight: integer;
begin
  result := ownerCanvas.TextHeight('Jp');
end;

function TFontMetrics.stringWidth(Value: String): integer;
begin
  result := ownerCanvas.TextWidth(Value);
end;

end.





