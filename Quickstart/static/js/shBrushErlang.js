/**
 * Erlang Brush for DP Syntax Highlighter by Alex Gorbatchev
 *
 *   http://code.google.com/p/syntaxhighlighter/
 *
 * Copyright (C) 2008 Darach Ennis.
 *
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General 
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option) 
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more 
 * details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to 
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA 
 */

dp.sh.Brushes.Erlang = function()
{
	var keywords = 
	 '-module -import -export -compile -type -spec -file -record catch' +
	 'orelse andalso bor bxor bsl bsr or xor div rem band and bnot not' +
	 'begin end if case of when receive after fun query test record'
	 'true false';
	
	var ppkeywords =
	 '-include -include_lib -define -undef -ifdef -ifndef -else -endif';
	 
	this.regexList = [
	  { regex: new RegExp("%.*$","gm"), css: 'comment' },
	  { regex: dp.sh.RegexLib.DoubleQuotedString, css: 'string' },
	  { regex: dp.sh.RegexLib.SingleQuotedString, css: 'variable' },	
	  { regex: new RegExp('[A-Z][A-Za-z0-9_@]*','gm'), css: 'variable' },
	  { regex: new RegExp('^_+[A-Za-z0-0_@]*','m'), css: 'variable' },
	  { regex: new RegExp('^-[a-z]*', 'gm'), css: 'preprocessor' },
	  { regex: new RegExp(this.GetKeywords(keywords),'gm'), css: 'keyword' }
		];

	this.CssClass = 'dp-erlang';
}

dp.sh.Brushes.Erlang.prototype	= new dp.sh.Highlighter();
dp.sh.Brushes.Erlang.Aliases	= ['erlang', 'erl', 'hrl', 'yrl'];
