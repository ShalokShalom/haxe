package cases;

class VsHaxeIssue648 extends DisplayTestCase {
	/**
		trace('Jeremy in $ci{-1-}ty');
		trace('Jérémy in $ci{-2-}ty');
	**/
	@:funcCode function test() {
		var diag = diagnostics().filter(d -> d.kind == DiagnosticKind.DKUnresolvedIdentifier);
		eq(2, diag.length);
		eq(diag[0].range.start.character, diag[1].range.start.character);
		eq(diag[0].range.end.character, diag[1].range.end.character);
	}
}
