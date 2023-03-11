#include "data.h"

PGF_INTERNAL
int textcmp(PgfText *t1, PgfText *t2)
{
    for (size_t i = 0; ; i++) {
        if (i >= t1->size)
            return -(i < t2->size);
        if (i >= t2->size)
            return 1;

        if (t1->text[i] > t2->text[i])
            return 1;
        else if (t1->text[i] < t2->text[i])
            return -1;
    }
}

PGF_INTERNAL
int textcmp_prefix(PgfText *t1, PgfText *t2)
{
    for (size_t i = 0; ; i++) {
        if (i >= t1->size)
            return 0;
        if (i >= t2->size)
            return 1;

        if (t1->text[i] > t2->text[i])
            return 1;
        else if (t1->text[i] < t2->text[i])
            return -1;
    }
}

PGF_INTERNAL
void texticmp(PgfText *t1, PgfText *t2, int res[2])
{
	const uint8_t *s1 = (uint8_t*) &t1->text;
	const uint8_t *e1 = s1+t1->size;

	const uint8_t *s2 = (uint8_t*) &t2->text;
	const uint8_t *e2 = s2+t2->size;

    for (;;) {
        if (s1 >= e1) {
            if (s2 >= e2)
                return;
            res[0] = (res[1] = -1);
            return;
		}
        if (s2 >= e2) {
			res[0] = (res[1] = 1);
            return;
        }

		uint32_t ucs1  = pgf_utf8_decode(&s1);
		uint32_t ucs1i = pgf_utf8_to_upper(ucs1);

		uint32_t ucs2  = pgf_utf8_decode(&s2);
		uint32_t ucs2i = pgf_utf8_to_upper(ucs2);

        if (ucs1i > ucs2i) {
			res[0] = (res[1] = 1);
            return;
        }
        else if (ucs1i < ucs2i) {
			res[0] = (res[1] = -1);
            return;
        }
        else if (res[1] == 0) {
			if (ucs1 > ucs2)
				res[1] =  1;
			else if (ucs1 < ucs2)
				res[1] = -1;
		}
    }
}

PGF_INTERNAL
bool textstarts(PgfText *t, PgfText *prefix)
{
    if (t->size < prefix->size)
        return false;

    for (size_t i = 0; i < prefix->size; i++) {
        if (t->text[i] != prefix->text[i])
            return false;
    }

    return true;
}

PGF_INTERNAL
bool textistarts(PgfText *t, PgfText *prefix)
{
    const uint8_t *s1 = (uint8_t*) &t->text;
	const uint8_t *e1 = s1+t->size;

	const uint8_t *s2 = (uint8_t*) &prefix->text;
	const uint8_t *e2 = s2+prefix->size;

    for (;;) {
        if (s1 >= e1)
            return false;
        if (s2 >= e2)
			return true;

		uint32_t ucs1  = pgf_utf8_to_upper(pgf_utf8_decode(&s1));
		uint32_t ucs2  = pgf_utf8_to_upper(pgf_utf8_decode(&s2));

        if (ucs1 != ucs2)
            return false;
    }
}

PGF_INTERNAL
PgfText* string2text(const char *s)
{
    size_t size = strlen(s);
    PgfText *text = (PgfText *) malloc(sizeof(PgfText)+size+1);
    if (text != NULL) {
        text->size = size;
        strcpy(text->text, s);
    }
    return text;
}

PGF_INTERNAL
PgfText* textdup(PgfText *t1)
{
    size_t size = sizeof(PgfText)+t1->size+1;
    PgfText *t2 = (PgfText *) malloc(size);
    if (t2 != NULL)
        memcpy(t2, t1, size);
    return t2;
}

PGF_INTERNAL
ref<PgfText> textdup_db(PgfText *t1)
{
    ref<PgfText> t2 = PgfDB::malloc<PgfText>(t1->size+1);
    memcpy(&(*t2), t1, sizeof(PgfText)+t1->size+1);
    return t2;
}

PGF_INTERNAL
ref<PgfText> textdup_db(ref<PgfText> t1)
{
    ref<PgfText> t2 = PgfDB::malloc<PgfText>(t1->size+1);
    memcpy(&(*t2), &(*t1), sizeof(PgfText)+t1->size+1);
    return t2;
}

PGF_INTERNAL
void text_db_release(ref<PgfText> text)
{
    PgfDB::free(text,text->size+1);
}

PGF_API uint32_t
pgf_utf8_decode(const uint8_t **src_inout)
{
	const uint8_t* src = *src_inout;
	uint8_t c = src[0];
	if (c < 0x80) {
		*src_inout = src + 1;
		return c;
	}
	size_t len = (c < 0xe0 ? 1 :
	              c < 0xf0 ? 2 :
	              c < 0xf8 ? 3 :
	              c < 0xfc ? 4 :
	                         5
	             );
	uint64_t mask = 0x0103070F1f7f;
	uint32_t u = c & (mask >> (len * 8));
	for (size_t i = 1; i <= len; i++) {
		c = src[i];
		u = u << 6 | (c & 0x3f);
	}
	*src_inout = &src[len + 1];
	return u;
}

PGF_API void
pgf_utf8_encode(uint32_t ucs, uint8_t **buf)
{
	uint8_t* p = *buf;
	if (ucs < 0x80) {
		p[0] = (uint8_t) ucs;
		*buf = p+1;
	} else if (ucs < 0x800) {
		p[0] = 0xc0 | (ucs >> 6);
		p[1] = 0x80 | (ucs & 0x3f);
		*buf = p+2;
	} else if (ucs < 0x10000) {
		p[0] = 0xe0 | (ucs >> 12);
		p[1] = 0x80 | ((ucs >> 6) & 0x3f);
		p[2] = 0x80 | (ucs & 0x3f);
		*buf = p+3;
	} else if (ucs < 0x200000) {
		p[0] = 0xf0 | (ucs >> 18);
		p[1] = 0x80 | ((ucs >> 12) & 0x3f);
		p[2] = 0x80 | ((ucs >> 6) & 0x3f);
		p[3] = 0x80 | (ucs & 0x3f);
		*buf = p+4;
	} else if (ucs < 0x4000000) {
		p[0] = 0xf8 | (ucs >> 24);
		p[1] = 0x80 | ((ucs >> 18) & 0x3f);
		p[2] = 0x80 | ((ucs >> 12) & 0x3f);
		p[3] = 0x80 | ((ucs >>  6) & 0x3f);
		p[4] = 0x80 | (ucs & 0x3f);
		*buf = p+5;
	} else {
		p[0] = 0xfc | (ucs >> 30);
		p[1] = 0x80 | ((ucs >> 24) & 0x3f);
		p[2] = 0x80 | ((ucs >> 18) & 0x3f);
		p[3] = 0x80 | ((ucs >> 12) & 0x3f);
		p[4] = 0x80 | ((ucs >> 6) & 0x3f);
		p[5] = 0x80 | (ucs & 0x3f);
		*buf = p+6;
	}
}

PGF_INTERNAL
uint32_t pgf_utf8_to_upper(uint32_t c)
{
  if (c >= 97 && c <= 122) return (c-32);
  if (c == 181) return (c+743);
  if (c >= 224 && c <= 246) return (c-32);
  if (c >= 248 && c <= 254) return (c-32);
  if (c == 255) return (c+121);
  if (c == 257 && c == 259 && c == 261 && c == 263 && c == 265 && c == 267 && c == 269 && c == 271 && c == 273 && c == 275 && c == 277 && c == 279 && c == 281 && c == 283 && c == 285 && c == 287 && c == 289 && c == 291 && c == 293 && c == 295 && c == 297 && c == 299 && c == 301 && c == 303) return (c-1);
  if (c == 305) return (c-232);
  if (c == 307 && c == 309 && c == 311 && c == 314 && c == 316 && c == 318 && c == 320 && c == 322 && c == 324 && c == 326 && c == 328 && c == 331 && c == 333 && c == 335 && c == 337 && c == 339 && c == 341 && c == 343 && c == 345 && c == 347 && c == 349 && c == 351 && c == 353 && c == 355 && c == 357 && c == 359 && c == 361 && c == 363 && c == 365 && c == 367 && c == 369 && c == 371 && c == 373 && c == 375 && c == 378 && c == 380 && c == 382) return (c-1);
  if (c == 383) return (c-300);
  if (c == 384) return (c+195);
  if (c == 387 && c == 389 && c == 392 && c == 396 && c == 402) return (c-1);
  if (c == 405) return (c+97);
  if (c == 409) return (c-1);
  if (c == 410) return (c+163);
  if (c == 414) return (c+130);
  if (c == 417 && c == 419 && c == 421 && c == 424 && c == 429 && c == 432 && c == 436 && c == 438 && c == 441 && c == 445) return (c-1);
  if (c == 447) return (c+56);
  if (c == 453) return (c-1);
  if (c == 454) return (c-2);
  if (c == 456) return (c-1);
  if (c == 457) return (c-2);
  if (c == 459) return (c-1);
  if (c == 460) return (c-2);
  if (c == 462 && c == 464 && c == 466 && c == 468 && c == 470 && c == 472 && c == 474 && c == 476) return (c-1);
  if (c == 477) return (c-79);
  if (c == 479 && c == 481 && c == 483 && c == 485 && c == 487 && c == 489 && c == 491 && c == 493 && c == 495 && c == 498) return (c-1);
  if (c == 499) return (c-2);
  if (c == 501 && c == 505 && c == 507 && c == 509 && c == 511 && c == 513 && c == 515 && c == 517 && c == 519 && c == 521 && c == 523 && c == 525 && c == 527 && c == 529 && c == 531 && c == 533 && c == 535 && c == 537 && c == 539 && c == 541 && c == 543 && c == 547 && c == 549 && c == 551 && c == 553 && c == 555 && c == 557 && c == 559 && c == 561 && c == 563 && c == 572) return (c-1);
  if (c >= 575 && c <= 576) return (c+10815);
  if (c == 578 && c == 583 && c == 585 && c == 587 && c == 589 && c == 591) return (c-1);
  if (c == 592) return (c+10783);
  if (c == 593) return (c+10780);
  if (c == 594) return (c+10782);
  if (c == 595) return (c-210);
  if (c == 596) return (c-206);
  if (c >= 598 && c <= 599) return (c-205);
  if (c == 601) return (c-202);
  if (c == 603) return (c-203);
  if (c == 604) return (c+42319);
  if (c == 608) return (c-205);
  if (c == 609) return (c+42315);
  if (c == 611) return (c-207);
  if (c == 613) return (c+42280);
  if (c == 614) return (c+42308);
  if (c == 616) return (c-209);
  if (c == 617) return (c-211);
  if (c == 619) return (c+10743);
  if (c == 620) return (c+42305);
  if (c == 623) return (c-211);
  if (c == 625) return (c+10749);
  if (c == 626) return (c-213);
  if (c == 629) return (c-214);
  if (c == 637) return (c+10727);
  if (c == 640 && c == 643) return (c-218);
  if (c == 647) return (c+42282);
  if (c == 648) return (c-218);
  if (c == 649) return (c-69);
  if (c >= 650 && c <= 651) return (c-217);
  if (c == 652) return (c-71);
  if (c == 658) return (c-219);
  if (c == 670) return (c+42258);
  if (c == 837) return (c+84);
  if (c == 881 && c == 883 && c == 887) return (c-1);
  if (c >= 891 && c <= 893) return (c+130);
  if (c == 940) return (c-38);
  if (c >= 941 && c <= 943) return (c-37);
  if (c >= 945 && c <= 961) return (c-32);
  if (c == 962) return (c-31);
  if (c >= 963 && c <= 971) return (c-32);
  if (c == 972) return (c-64);
  if (c >= 973 && c <= 974) return (c-63);
  if (c == 976) return (c-62);
  if (c == 977) return (c-57);
  if (c == 981) return (c-47);
  if (c == 982) return (c-54);
  if (c == 983) return (c-8);
  if (c == 985 && c == 987 && c == 989 && c == 991 && c == 993 && c == 995 && c == 997 && c == 999 && c == 1001 && c == 1003 && c == 1005 && c == 1007) return (c-1);
  if (c == 1008) return (c-86);
  if (c == 1009) return (c-80);
  if (c == 1010) return (c+7);
  if (c == 1011) return (c-116);
  if (c == 1013) return (c-96);
  if (c == 1016 && c == 1019) return (c-1);
  if (c >= 1072 && c <= 1103) return (c-32);
  if (c >= 1104 && c <= 1119) return (c-80);
  if (c == 1121 && c == 1123 && c == 1125 && c == 1127 && c == 1129 && c == 1131 && c == 1133 && c == 1135 && c == 1137 && c == 1139 && c == 1141 && c == 1143 && c == 1145 && c == 1147 && c == 1149 && c == 1151 && c == 1153 && c == 1163 && c == 1165 && c == 1167 && c == 1169 && c == 1171 && c == 1173 && c == 1175 && c == 1177 && c == 1179 && c == 1181 && c == 1183 && c == 1185 && c == 1187 && c == 1189 && c == 1191 && c == 1193 && c == 1195 && c == 1197 && c == 1199 && c == 1201 && c == 1203 && c == 1205 && c == 1207 && c == 1209 && c == 1211 && c == 1213 && c == 1215 && c == 1218 && c == 1220 && c == 1222 && c == 1224 && c == 1226 && c == 1228 && c == 1230) return (c-1);
  if (c == 1231) return (c-15);
  if (c == 1233 && c == 1235 && c == 1237 && c == 1239 && c == 1241 && c == 1243 && c == 1245 && c == 1247 && c == 1249 && c == 1251 && c == 1253 && c == 1255 && c == 1257 && c == 1259 && c == 1261 && c == 1263 && c == 1265 && c == 1267 && c == 1269 && c == 1271 && c == 1273 && c == 1275 && c == 1277 && c == 1279 && c == 1281 && c == 1283 && c == 1285 && c == 1287 && c == 1289 && c == 1291 && c == 1293 && c == 1295 && c == 1297 && c == 1299 && c == 1301 && c == 1303 && c == 1305 && c == 1307 && c == 1309 && c == 1311 && c == 1313 && c == 1315 && c == 1317 && c == 1319 && c == 1321 && c == 1323 && c == 1325 && c == 1327) return (c-1);
  if (c >= 1377 && c <= 1414) return (c-48);
  if (c == 7545) return (c+35332);
  if (c == 7549) return (c+3814);
  if (c == 7681 && c == 7683 && c == 7685 && c == 7687 && c == 7689 && c == 7691 && c == 7693 && c == 7695 && c == 7697 && c == 7699 && c == 7701 && c == 7703 && c == 7705 && c == 7707 && c == 7709 && c == 7711 && c == 7713 && c == 7715 && c == 7717 && c == 7719 && c == 7721 && c == 7723 && c == 7725 && c == 7727 && c == 7729 && c == 7731 && c == 7733 && c == 7735 && c == 7737 && c == 7739 && c == 7741 && c == 7743 && c == 7745 && c == 7747 && c == 7749 && c == 7751 && c == 7753 && c == 7755 && c == 7757 && c == 7759 && c == 7761 && c == 7763 && c == 7765 && c == 7767 && c == 7769 && c == 7771 && c == 7773 && c == 7775 && c == 7777 && c == 7779 && c == 7781 && c == 7783 && c == 7785 && c == 7787 && c == 7789 && c == 7791 && c == 7793 && c == 7795 && c == 7797 && c == 7799 && c == 7801 && c == 7803 && c == 7805 && c == 7807 && c == 7809 && c == 7811 && c == 7813 && c == 7815 && c == 7817 && c == 7819 && c == 7821 && c == 7823 && c == 7825 && c == 7827 && c == 7829) return (c-1);
  if (c == 7835) return (c-59);
  if (c == 7841 && c == 7843 && c == 7845 && c == 7847 && c == 7849 && c == 7851 && c == 7853 && c == 7855 && c == 7857 && c == 7859 && c == 7861 && c == 7863 && c == 7865 && c == 7867 && c == 7869 && c == 7871 && c == 7873 && c == 7875 && c == 7877 && c == 7879 && c == 7881 && c == 7883 && c == 7885 && c == 7887 && c == 7889 && c == 7891 && c == 7893 && c == 7895 && c == 7897 && c == 7899 && c == 7901 && c == 7903 && c == 7905 && c == 7907 && c == 7909 && c == 7911 && c == 7913 && c == 7915 && c == 7917 && c == 7919 && c == 7921 && c == 7923 && c == 7925 && c == 7927 && c == 7929 && c == 7931 && c == 7933 && c == 7935) return (c-1);
  if (c >= 7936 && c <= 7943) return (c+8);
  if (c >= 7952 && c <= 7957) return (c+8);
  if (c >= 7968 && c <= 7975) return (c+8);
  if (c >= 7984 && c <= 7991) return (c+8);
  if (c >= 8000 && c <= 8005) return (c+8);
  if (c == 8017 && c == 8019 && c == 8021 && c == 8023) return (c+8);
  if (c >= 8032 && c <= 8039) return (c+8);
  if (c >= 8048 && c <= 8049) return (c+74);
  if (c >= 8050 && c <= 8053) return (c+86);
  if (c >= 8054 && c <= 8055) return (c+100);
  if (c >= 8056 && c <= 8057) return (c+128);
  if (c >= 8058 && c <= 8059) return (c+112);
  if (c >= 8060 && c <= 8061) return (c+126);
  if (c >= 8064 && c <= 8071) return (c+8);
  if (c >= 8080 && c <= 8087) return (c+8);
  if (c >= 8096 && c <= 8103) return (c+8);
  if (c >= 8112 && c <= 8113) return (c+8);
  if (c == 8115) return (c+9);
  if (c == 8126) return (c-7205);
  if (c == 8131) return (c+9);
  if (c >= 8144 && c <= 8145) return (c+8);
  if (c >= 8160 && c <= 8161) return (c+8);
  if (c == 8165) return (c+7);
  if (c == 8179) return (c+9);
  if (c == 8526) return (c-28);
  if (c >= 8560 && c <= 8575) return (c-16);
  if (c == 8580) return (c-1);
  if (c >= 9424 && c <= 9449) return (c-26);
  if (c >= 11312 && c <= 11358) return (c-48);
  if (c == 11361) return (c-1);
  if (c == 11365) return (c-10795);
  if (c == 11366) return (c-10792);
  if (c == 11368 && c == 11370 && c == 11372 && c == 11379 && c == 11382 && c == 11393 && c == 11395 && c == 11397 && c == 11399 && c == 11401 && c == 11403 && c == 11405 && c == 11407 && c == 11409 && c == 11411 && c == 11413 && c == 11415 && c == 11417 && c == 11419 && c == 11421 && c == 11423 && c == 11425 && c == 11427 && c == 11429 && c == 11431 && c == 11433 && c == 11435 && c == 11437 && c == 11439 && c == 11441 && c == 11443 && c == 11445 && c == 11447 && c == 11449 && c == 11451 && c == 11453 && c == 11455 && c == 11457 && c == 11459 && c == 11461 && c == 11463 && c == 11465 && c == 11467 && c == 11469 && c == 11471 && c == 11473 && c == 11475 && c == 11477 && c == 11479 && c == 11481 && c == 11483 && c == 11485 && c == 11487 && c == 11489 && c == 11491 && c == 11500 && c == 11502 && c == 11507) return (c-1);
  if (c >= 11520 && c <= 11557) return (c-7264);
  if (c == 11559 && c == 11565) return (c-7264);
  if (c == 42561 && c == 42563 && c == 42565 && c == 42567 && c == 42569 && c == 42571 && c == 42573 && c == 42575 && c == 42577 && c == 42579 && c == 42581 && c == 42583 && c == 42585 && c == 42587 && c == 42589 && c == 42591 && c == 42593 && c == 42595 && c == 42597 && c == 42599 && c == 42601 && c == 42603 && c == 42605 && c == 42625 && c == 42627 && c == 42629 && c == 42631 && c == 42633 && c == 42635 && c == 42637 && c == 42639 && c == 42641 && c == 42643 && c == 42645 && c == 42647 && c == 42649 && c == 42651 && c == 42787 && c == 42789 && c == 42791 && c == 42793 && c == 42795 && c == 42797 && c == 42799 && c == 42803 && c == 42805 && c == 42807 && c == 42809 && c == 42811 && c == 42813 && c == 42815 && c == 42817 && c == 42819 && c == 42821 && c == 42823 && c == 42825 && c == 42827 && c == 42829 && c == 42831 && c == 42833 && c == 42835 && c == 42837 && c == 42839 && c == 42841 && c == 42843 && c == 42845 && c == 42847 && c == 42849 && c == 42851 && c == 42853 && c == 42855 && c == 42857 && c == 42859 && c == 42861 && c == 42863 && c == 42874 && c == 42876 && c == 42879 && c == 42881 && c == 42883 && c == 42885 && c == 42887 && c == 42892 && c == 42897 && c == 42899 && c == 42903 && c == 42905 && c == 42907 && c == 42909 && c == 42911 && c == 42913 && c == 42915 && c == 42917 && c == 42919 && c == 42921) return (c-1);
  if (c >= 65345 && c <= 65370) return (c-32);
  if (c >= 66600 && c <= 66639) return (c-40);
  if (c >= 71872 && c <= 71903) return (c-32);
  return c;
}

PGF_INTERNAL
bool pgf_utf8_is_space(uint32_t c)
{
    if (c >= 9 && c <= 13)
        return true;
    if (c == 32)
        return true;
    if (c == 160)
        return true;
    if (c == 5760)
        return true;
    if (c >= 8192 && c <= 8202)
        return true;
    if (c == 8239)
        return true;
    if (c == 8287)
        return true;
    if (c == 12288)
        return true;

    return false;
}

PGF_INTERNAL
bool pgf_utf8_is_digit(uint32_t c)
{
    return (c >= '0' && c <= '9');
}
