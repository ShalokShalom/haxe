package cases.issues;

class Issue11849 extends TestCase {
	function test(_) {
		var content = getTemplate("issues/Issue11849/Main.hx");
		var transform = Markers.parse(content);
		vfs.putContent("Main.hx", transform.source);

		var args = ["-main", "Main"];
		runHaxe(args);
		assertSuccess();

		runHaxeJsonCb(args, DisplayMethods.Hover, {file: new FsPath("Main.hx"), offset: transform.offset(1)}, res -> {
			Assert.equals(Local, res.item.kind);
			Assert.equals("bar", res.item.args.name);
		});
		assertSuccess();
	}
}
