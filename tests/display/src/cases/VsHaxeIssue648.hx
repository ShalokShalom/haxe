package cases;

class VsHaxeIssue648 extends DisplayTestCase {
	/**
		trace('Jeremy $te{-1-}st');
		trace('Jérémy $te{-2-}st');
		trace('名 字  $te{-3-}st');
		trace('zя���� $te{-4-}st');
		trace('😀 😀  $te{-5-}st');
	**/
	@:funcCode function test() {
		var diag = diagnostics().filter(d -> d.kind == DiagnosticKind.DKUnresolvedIdentifier);
		eq(5, diag.length);

		for (i in 1...4) {
			eq(diag[0].range.start.character, diag[i].range.start.character);
			eq(diag[0].range.end.character, diag[i].range.end.character);
		}
	}
}
