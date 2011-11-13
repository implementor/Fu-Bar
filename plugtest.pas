library plugtest;

{$longstrings on}

procedure invoke;cdecl;
begin
  Writeln('Invoked');
end;

procedure transmit(const s: pchar);cdecl;
begin
  Writeln('Received: ',string(s));
end;

exports
  invoke name 'fubar_plugin_invoke',
  transmit name 'fubar_plugin_transmit';
  
end.
