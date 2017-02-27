program GraphLayout;

uses
  Forms,
  Utils in 'Utils.pas',
  Graph in 'Graph.pas' {frmGraph};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;

  Application.Initialize;
  Application.CreateForm(TfrmGraph, frmGraph);
  Application.Run;
end.
