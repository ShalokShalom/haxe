package cases.issues;

class Issue11184 extends TestCase {
	// Disabled for now until #11184 is actually fixed, likely by #11220
	// function testDiagnostics(_) {
	// 	vfs.putContent("Main.hx", getTemplate("issues/Issue11184/Main.hx"));
	// 	var args = ["-main", "Main", "-js", "bin/test.js"];

	// 	runHaxeJsonCb(args, DisplayMethods.Diagnostics, {file: new FsPath("Main.hx")}, res -> {
	// 		Assert.equals(1, res.length);
	// 		Assert.equals(1, res[0].diagnostics.length);
	// 		Assert.equals(res[0].diagnostics[0].args, "Cannot use Void as value");
	// 	});

	// 	runHaxe(args);
	// 	Assert.isTrue(hasErrorMessage("Cannot use Void as value"));
	// 	runHaxe(args);
	// 	Assert.isTrue(hasErrorMessage("Cannot use Void as value"));
	// }

	// function testLegacyDiagnostics(_) {
	// 	vfs.putContent("Main.hx", getTemplate("issues/Issue11184/Main.hx"));
	// 	var args = ["-main", "Main", "-js", "bin/test.js"];
	// 	runHaxe(args.concat(["--display", "Main.hx@0@diagnostics"]));
	// 	final diagnostics = haxe.Json.parse(lastResult.stderr)[0].diagnostics;
	// 	Assert.equals(diagnostics[0].args, "Cannot use Void as value");
	// 	runHaxe(args);
	// 	Assert.isTrue(hasErrorMessage("Cannot use Void as value"));
	// 	runHaxe(args);
	// 	Assert.isTrue(hasErrorMessage("Cannot use Void as value"));
	// }
}
