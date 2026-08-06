// Language names and ISO-639 codes (both 3-letter and 2-letter codes)
// See http://en.wikipedia.org/wiki/List_of_ISO_639-1_codes

var languages =
    function() {
	function lang1(namecode2) {
	    function lang(code,name,code2) {
		return {code:code, name:name, code2:code2}
	    }
	    var nc=namecode2.split(":")
	    var name=nc[0]
	    var ws=name.split("/")
	    var code2=nc.length>1 ? nc[1] : ""
	    return ws.length==1 ? lang(name.substr(0,3),name,code2)
	                        : lang(ws[0],ws[1],code2)
	}
	var ls
	//  [ISO-639-2 code "/"] language name ":" ISO 639-1 code
    ls=["Afrikaans:af","Sqi/Albanian:sq","Amharic:am","Arabic:ar",
        "Hye/Armenian:hy","Eus/Basque/eu","Bel/Belarusian:be","Bulgarian:bg",
        "Catalan:ca","Chinese:zh","Czech:cs","Danish:da",
        "Dutch:nl","English:en","Estonian:et","Fao/Faroese:fo",
        "Finnish:fi","French:fr","Gla/Gaelic:gd","German:de",
        "Greek:el","Hebrew:he","Hindi:hi","Hungarian/hu",
        "Icelandic:is","Ina/Interlingua:ia","Gle/Irish:ga","Italian:it",
        "Jpn/Japanese:ja","Kazakh:kk","Korean:ko","Latin:la",
        "Lav/Latvian:lv","Mkd/Macedonian:mk","Mlt/Maltese:mt","Mongolian:mn",
        "Nepali:ne","Norwegian Bokmål:nb","Nno/Norwegian Nynorsk:nn","Pes/Persian:fa",
        "Polish:pl","Portuguese:pt","Pnb/Punjabi:pa","Ron/Romanian:ro",
        "Russian:ru","Scots:sco","Slv/Slovenian:sl","Somali:so",
        "Snd/Sindhi:sd","Spanish:es","Swahili:sw","Swedish:sv",
        "Thai:th","Turkish:tr","Ukrainian:uk","Urdu:ur",
        "Zulu:zu"]
	// GF uses nonstd 3-letter codes? Pes/Persian:fa, Pnb/Punjabi:pa
	return map(lang1,ls)
    }()

var langname={}
var langcode={}
var langcode2={}
var langcode3={}
for(var i in languages) {
    langname[languages[i].code]=languages[i].name
    langcode[languages[i].name]=languages[i]
    langcode2[languages[i].code]=languages[i].code2
    langcode3[languages[i].code2]=languages[i].code
}

function concname(code) { return langname[code] || code; }
function alangcode(code) { return langcode2[code] || code; }

// Add a country code to the language code
function add_country(code) {
    switch(code) {
    case "en": return "en-US"  // "en-scotland" // or "en-GB"
    case "sv": return "sv-SE"
    case "fr": return "fr-FR"
    case "de": return "de-DE"
    case "fi": return "fi-FI"
    case "zh": return "zh-CN"
    case "hi": return "hi-IN"
    case "es": return "es-ES"
    case "it": return "it-IT"
    case "bg": return "bg-BG" // ?
    case "da": return "da-DK"
    case "nb": return "nb-NO"
    case "nl": return "nl-NL"
    case "ja": return "ja-JP"
    case "ro": return "ja-RO"
    case "el": return "el-GR"
    case "th": return "th-TH"
    // ...
    default: return code
    }
}
