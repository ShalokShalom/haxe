function main() {
	#if nofail
	var test = "test";
	#end
	trace('Jeremy $test');
	trace('Jérémy $test');
	trace('名 字 $test');
	trace('zя���� $test abcdefghijk');
	trace('���� $test abcdefghijk');
	trace('zя $test abcdefghijk');
	trace('😀 😀 $test abcdefghijk');
	trace('😀 😀 zя���� $test abcdefghijk');
}
