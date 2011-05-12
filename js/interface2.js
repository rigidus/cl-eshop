/* JSON stringify */
eval(function(p,a,c,k,e,r){e=function(c){return(c<a?'':e(parseInt(c/a)))+((c=c%a)>35?String.fromCharCode(c+29):c.toString(36))};if(!''.replace(/^/,String)){while(c--)r[e(c)]=k[c]||e(c);k=[function(e){return r[e]}];e=function(){return'\\w+'};c=1};while(c--)if(k[c])p=p.replace(new RegExp('\\b'+e(c)+'\\b','g'),k[c]);return p}('3(!l.o){l.o={}}(5(){5 f(n){7 n<10?\'0\'+n:n}3(6 11.q.p!==\'5\'){11.q.p=5(a){7 1p(l.1o())?l.1v()+\'-\'+f(l.1x()+1)+\'-\'+f(l.1z())+\'T\'+f(l.1B())+\':\'+f(l.1C())+\':\'+f(l.1E())+\'Z\':y};J.q.p=1w.q.p=1y.q.p=5(a){7 l.1o()}}w e=/[\\1A\\1n\\1m-\\1l\\1k\\1h\\1e\\1c-\\1b\\1a-\\19\\18-\\17\\15\\14-\\13]/g,G=/[\\\\\\"\\1F-\\1G\\1q-\\1r\\1n\\1m-\\1l\\1k\\1h\\1e\\1c-\\1b\\1a-\\19\\18-\\17\\15\\14-\\13]/g,8,C,12={\'\\b\':\'\\\\b\',\'\\t\':\'\\\\t\',\'\\n\':\'\\\\n\',\'\\f\':\'\\\\f\',\'\\r\':\'\\\\r\',\'"\':\'\\\\"\',\'\\\\\':\'\\\\\\\\\'},m;5 H(b){G.V=0;7 G.P(b)?\'"\'+b.D(G,5(a){w c=12[a];7 6 c===\'K\'?c:\'\\\\u\'+(\'X\'+a.W(0).U(16)).Y(-4)})+\'"\':\'"\'+b+\'"\'}5 E(a,b){w i,k,v,h,B=8,9,2=b[a];3(2&&6 2===\'x\'&&6 2.p===\'5\'){2=2.p(a)}3(6 m===\'5\'){2=m.L(b,a,2)}1J(6 2){A\'K\':7 H(2);A\'M\':7 1p(2)?J(2):\'y\';A\'1s\':A\'y\':7 J(2);A\'x\':3(!2){7\'y\'}8+=C;9=[];3(S.q.U.1t(2)===\'[x 1u]\'){h=2.h;z(i=0;i<h;i+=1){9[i]=E(i,2)||\'y\'}v=9.h===0?\'[]\':8?\'[\\n\'+8+9.I(\',\\n\'+8)+\'\\n\'+B+\']\':\'[\'+9.I(\',\')+\']\';8=B;7 v}3(m&&6 m===\'x\'){h=m.h;z(i=0;i<h;i+=1){k=m[i];3(6 k===\'K\'){v=E(k,2);3(v){9.1d(H(k)+(8?\': \':\':\')+v)}}}}Q{z(k 1f 2){3(S.1g.L(2,k)){v=E(k,2);3(v){9.1d(H(k)+(8?\': \':\':\')+v)}}}}v=9.h===0?\'{}\':8?\'{\\n\'+8+9.I(\',\\n\'+8)+\'\\n\'+B+\'}\':\'{\'+9.I(\',\')+\'}\';8=B;7 v}}3(6 o.O!==\'5\'){o.O=5(a,b,c){w i;8=\'\';C=\'\';3(6 c===\'M\'){z(i=0;i<c;i+=1){C+=\' \'}}Q 3(6 c===\'K\'){C=c}m=b;3(b&&6 b!==\'5\'&&(6 b!==\'x\'||6 b.h!==\'M\')){1i 1j 1D(\'o.O\');}7 E(\'\',{\'\':a})}}3(6 o.N!==\'5\'){o.N=5(c,d){w j;5 R(a,b){w k,v,2=a[b];3(2&&6 2===\'x\'){z(k 1f 2){3(S.1g.L(2,k)){v=R(2,k);3(v!==1H){2[k]=v}Q{1I 2[k]}}}}7 d.L(a,b,2)}c=J(c);e.V=0;3(e.P(c)){c=c.D(e,5(a){7\'\\\\u\'+(\'X\'+a.W(0).U(16)).Y(-4)})}3(/^[\\],:{}\\s]*$/.P(c.D(/\\\\(?:["\\\\\\/1K]|u[0-1L-1M-F]{4})/g,\'@\').D(/"[^"\\\\\\n\\r]*"|1N|1O|y|-?\\d+(?:\\.\\d*)?(?:[1P][+\\-]?\\d+)?/g,\']\').D(/(?:^|:|,)(?:\\s*\\[)+/g,\'\'))){j=1Q(\'(\'+c+\')\');7 6 d===\'5\'?R({\'\':j},\'\'):j}1i 1j 1R(\'o.N\');}}}());',62,116,'||value|if||function|typeof|return|gap|partial||||||||length||||this|rep||JSON|toJSON|prototype||||||var|object|null|for|case|mind|indent|replace|str||escapable|quote|join|String|string|call|number|parse|stringify|test|else|walk|Object||toString|lastIndex|charCodeAt|0000|slice|||Date|meta|uffff|ufff0|ufeff||u206f|u2060|u202f|u2028|u200f|u200c|push|u17b5|in|hasOwnProperty|u17b4|throw|new|u070f|u0604|u0600|u00ad|valueOf|isFinite|x7f|x9f|boolean|apply|Array|getUTCFullYear|Number|getUTCMonth|Boolean|getUTCDate|u0000|getUTCHours|getUTCMinutes|Error|getUTCSeconds|x00|x1f|undefined|delete|switch|bfnrt|9a|fA|true|false|eE|eval|SyntaxError'.split('|'),0,{}))

var w;


/* === Javascript Trim === */
String.prototype.trim = function() {
	return this.replace(/^\s+|\s+$/, '');
};

/* === Поиск по текст, независмый от регистра === */
$.extend($.expr[":"], {
	"Contains": function(elem, i, match, array) {
		return (elem.textContent || elem.innerText || "").toLowerCase().indexOf((match[3] || "").toLowerCase()) >=
		0;
	}
});

function reloadPage() {
	if ($.browser.msie && $.browser.version < 7) {
		document.body.style.display = "inline";
		document.body.style.display = "block";
	}
}

/* фикс png24 в IE6 для img */
function fixPNG(element) {
	if (/MSIE (5\.5|6).+Win/.test(navigator.userAgent)) {
		var src;
		if (element.tagName == 'IMG') {
			if (/\.png$/.test(element.src)) {
				src = element.src;
				element.src = "img/ico/blank.gif";
			}
		}
		else {
			src = element.currentStyle.backgroundImage.match(/url\("(.+\.png)"\)/i);
			if (src) {
				src = src[1];
				element.runtimeStyle.backgroundImage = "none";
			}
		}
		if (src) element.runtimeStyle.filter = "progid:DXImageTransform.Microsoft.AlphaImageLoader(src='" + src + "',sizingMethod='scale')";
	}
}


/* инициалзация слайдера */
function dailySliderInitCallback(carousel) {
	current = $(carousel.container);
	$(current).siblings('.slider-pager').find('a').unbind('click').bind('click', function() {
		carousel.scroll($.jcarousel.intval($(this).text()));
		return false;
	});
};
/* рефреш слайдера по клику */
function dailySliderItemLoadCallback(carousel) {
	current = $(carousel.container);
	$(current).siblings('.slider-pager').find('li').removeClass('active').end().find('li').eq(carousel.first - 1).addClass('active');
};
/* инициалзация слайдера */
function bestSliderInitCallback(carousel) {
	current = $(carousel.container);
	pager = $(current).parents('.best-items').find('.slider-pager');
	pager.find('li').not('.all').remove();
	for (i=carousel.size();i>=1;i--) {
		pager.prepend("<li><a href=\"#"+i+"\">"+i+"</a></li>");
	}
	$(pager).find('li').not('.all').find('a').unbind('click').bind('click', function() {
		carousel.scroll($.jcarousel.intval($(this).text()));
		return false;
	});
};
/* рефреш слайдера по клику */
function bestSliderItemLoadCallback(carousel) {
	current = $(carousel.container);
	$(current).parents('.best-items').find('.slider-pager').find('li').removeClass('active').end().find('li').eq(carousel.first - 1).addClass('active');
};

function closeFancy() {
	$.fancybox.close();
};

/* показ попапа */
function showPopup(current,where) {
	$(".popup").hide();
	$("#" + where).css("visibility", "hidden").show();
	var h = Math.round($("html").height()) / 2 - Math.round($("#" + where).height() / 2);
	$("#" + where).css("margin-top", h).css("visibility", "visible");
	if (!$.browser.msie) {$("#" + where).animate({opacity: 0}, 0);}
	$(".overlay").stop().show().animate({
		opacity: 0.4
	}, 150, 'swing', function(){
		if (!$.browser.msie) {
			$("#" + where).stop().animate({
				opacity: 1
			},250, 'swing', function(){
				initPopupAfterShow(current,where);
			});
		} else {
			$("#" + where).show();
			initPopupAfterShow(current,where);
		}
	});
	$("#" + where).find(":input").eq(0).focus();
}
/* показ тултипа */
function showTooltip(current,where) {
	$(".popup").hide();
	$("#" + where).show();
	initPopupAfterShow(current,where);
}
/* прятать попап */
function hidePopup(where){
	if (!$.browser.msie) {
		$(where).stop().fadeOut(200);
		$('.overlay').stop().fadeOut(200);
	} else {
		$(where).hide();
		$('.overlay').hide();
	}
}
function hideTooltip(where){
	$(where).hide();
	$('.overlay').hide();
}
/* после появления попапа */
function initPopupAfterShow(current,where) {
	if ($("#" + where).hasClass('popup-accReplace')) {
		/* если выбор аксессуара */
		initAccReplace($(current).parents('.item'),$("#" + where));
	}
}
/* клик по выбору аксессуара */
function initAccReplace(target,current){
	$(current).find('.item').each(function(){
		$(this).find('.pic a,.h2 a').unbind('click').click(function(){
			$(this).parents('.acc-scroll').find('.item').removeClass('item-active');
			$(this).parents('.item').addClass('item-active');
			$(target).find('.pic').html($(this).parents('.item').find('.pic').html());
			$(target).find('.info').html($(this).parents('.item').find('.info').html());
			refreshPriceAcc();
			hidePopup(current);
			return false;
		});
	});
}

/* инит скриптов аксессуаров */
function initPriceAcc(){
	$('.accessories').each(function(){
		var tmp = 99;
		$(this).find('.catalog-acc .item').each(function(){
			tmp--;
			$(this).css("z-index",tmp);
		});
		$(this).find('.catalog .checkbox').unbind('click').click(function(){
			if ($(this).is(":checked")) {
				$(this).parents('.item').find('.pic').stop().animate({opacity: 1},150);
			} else {
				$(this).parents('.item').find('.pic').stop().animate({opacity: 0.5},150);
			}
			refreshPriceAcc();
		});
	});
}
/* обновление цен аксессуаров */
function refreshPriceAcc(){
	$('.accessories').each(function(){
		var price1 = 0;
		var price2 = 0;
		$(this).find('.catalog-acc .item').each(function(){
			if (($(this).find('.cb input:checkbox').length == 0) || ($(this).find('.cb input:checked').length > 0)) {
				if ($(this).find('.price-1').length > 0) {
					price1 += parseInt($(this).find('.price-1').text().replace(' ', ''));
				}
				if ($(this).find('.price-2').length > 0) {
					price2 += parseInt($(this).find('.price-2').text().replace(' ', ''));
				}
			}
		});
		$(this).find('.item-buy').each(function(){
			$(this).find('.price-1').text(price1);
			$(this).find('.price-2').text(price1 + price2);
			$(this).find('.price-3').text(price2);
		});
	});
}

function refreshWidth() {
	w = parseInt($('.container').width());
	$(".best-items .slider").each(function(){
		if ($(this).is(":visible")) {
			if ($(this).parents('.best-items').hasClass('acc-tab')) {
				w = Math.floor(w * 0.75);
			}
			$(this).width(w + "px").find('.page').each(function(){
				$(this).width(w + "px");
			});
			$(this).jcarousel({
				initCallback: bestSliderInitCallback,
				itemLoadCallback: bestSliderItemLoadCallback,
				scroll: 1,
				buttonNextHTML: '<p></p>',
				buttonPrevHTML: '<p></p>'
			});
		}
	});
}



/* Тут ядерная бомба. Дата, когда время жизни куки истекает, проставлена руками
	 TODO: генерировать expires data*/
/*function rSetCookie (name, value) {
	document.cookie= name+"="+encodeURIComponent(value)+"; path=/; expires=Mon, 15-Oct-2011 00:00:00 GMT";
}*/

function rSetCookie(c_name, value){
	var exdate=new Date();
	exdate.setDate(exdate.getDate() + 30);
	var c_value=encodeURIComponent(value) + ("; expires="+exdate.toUTCString());
	document.cookie=c_name + "=" + c_value;
}

function rGetCookie(name) {
	var cookie = " " + document.cookie;
	var search = " " + name + "=";
	var setStr = null;
	var offset = 0;
	var end = 0;
	var rCart = new Array();
        var rUser = new Object();
	if (cookie.length > 0) {
		offset = cookie.indexOf(search);
		if (offset != -1) {
			offset += search.length;
			end = cookie.indexOf(";", offset)
			if (end == -1) {
				end = cookie.length;
			}
			try
			{
				setStr = decodeURIComponent(cookie.substring(offset, end));
				if(setStr == "") {
					setStr = null;
				}
  			}
			catch(err)
  			{
				setStr = null;
				if(name == 'cart') {
					rSetCookie('cart', JSON.stringify(rCart));
				}
				if(name == 'user') {
					rSetCookie('user', JSON.stringify(rUser));
				}
				return(setStr);
  			}
		}
	}
	return(setStr);
}

function rSave(rCart) {
	rSetCookie('cart', JSON.stringify(rCart));
}

function rCalc() {
	var rCart = eval(rGetCookie('cart'));
	var rUser = eval("("+rGetCookie('user')+")");
	var sum = 0;
	var cnt = 0;
	if (rCart) {
		for (i = 0; i < rCart.length; i++) {
			sum += rCart[i].price * rCart[i].count;
			cnt += rCart[i].count;
		}
	}
	$('.cart,.your-order').each(function(){
		var current = $(this).find('.cart-link a,.total');
		if(cnt > 0) {
			if (cnt == 1) {
				$(current).html('<i class="count"></i>&nbsp;товар на <big class="sum"></big> руб.');
			}
			else
				if (cnt > 4) {
					$(current).html('<i class="count"></i>&nbsp;товаров на <big class="sum"></big> руб.');
				}
				else {
					$(current).html('<i class="count"></i>&nbsp;товара на <big class="sum"></big> руб.');
				}
			$(current).find('.sum').html(sum);

			// ->> жесткий костыль для случая, когда rUser не определен в куках или определен странных образом
			if(rUser && 'delivery' in rUser){
				if (rUser.delivery.deliverytype == 'courier') {
					$(this).find('.delivery-price').html('Стоимость курьерской доставки — <big>300</big> руб. в пределах КАД<br> Самовывоз — бесплатно!');
				}
				else{
					$(this).find('.delivery-price').html('Самовывоз — <big>бесплатно</big>');
				}	
			}
			$(current).find('.count').html(cnt);
		}
		else{
			$(current).html('Нет товаров');
		}
	});
}


function rAddCart(id, group_id, name, price, count, item_link,img_link) {

	if(!count) 
		count = 1;
	var rCart = new Array();
	if (eval(rGetCookie('cart'))) {
		rCart = eval(rGetCookie('cart'));
	}
	var present = false;
	for (i = 0; i < rCart.length; i++) {
		if (id == rCart[i].id) {
			present = true;
			rCart[i].count += count;
		}
	}
	if (!present) {
		var tmp = {
			"id" : id,
			"group_id" : group_id,
			"name" : name,
			"price" : price,
			"count" : count,
			"item_link" : item_link,
			"img_link" : img_link
		}
		rCart.push(tmp);
	}
	rSave(rCart);
	rCalc();
	rCartReDraw();
	return false;
}

function rDelCart(id) {
	var rCart = eval(rGetCookie('cart'));
	var rNewCart = new Array();
	for (i = 0; i < rCart.length; i++) {
		if (id != rCart[i].id) {
			rNewCart.push(rCart[i]);
		}
	}
	rSave(rNewCart);
	rCalc();
	//rCartReDraw();
	return false;
}

function rCartReDraw() {
	var rCart = eval(rGetCookie('cart'));
	if (rCart) {
		if (0 == rCart.length) {
			$('.cart').each (function(){
				$(this).find('.cart-has').hide();
				$(this).find('.cart-blank').show();
			});
		} else {
			$('.cart').each (function(){
				$(this).find('.cart-has').show();
				$(this).find('.cart-blank').hide();
			});
		}
		$('.catalog-cart').find('.item').not('.item-template').remove();
		if ($('.catalog-cart').length > 0) {
			for (i = 0; i < rCart.length; i++) {
				var tmp = $('.catalog-cart').find('.item-template').clone();
				tmp.find('.num').text(i+1);
				tmp.find('.pic a').attr('href',rCart[i].item_link);
				tmp.find('.pic img').attr('src',rCart[i].img_link);
				tmp.find('.h2 a').text(rCart[i].name).attr('href',rCart[i].item_link);
				tmp.find('.price span').text(rCart[i].price);
				if (parseInt(rCart[i].count) > 1) {
					tmp.find('.count').text('x' + rCart[i].count);
				}
				var cur = rCart[i];
				(function(cur) {
					$(tmp).find('.delete').unbind('click').click(function(){
						$(this).parents('.item').addClass('item-deleted');
						$(this).parents('.item').find('.pic').animate({opacity: 0.5}, 0);
						rDelCart(cur.id);
						return false;
					});
					$(tmp).find('.return').unbind('click').click(function(){
						return rAddCart(cur.id, cur.group_id, cur.name, cur.price, cur.count,cur.item_link,cur.img_link);
					});
				})(cur);
				tmp.removeClass('item-template').appendTo('.catalog-cart')
			};
		}
	}
}

function rCartReDraw2() {
	var sum = 0;
	var sumold = 0;
	var cnt = 0;
	var tmpcnt = 0;
	var rCart = eval(rGetCookie('cart'));
	$('.catalog-cart2 .item').each(function(){
		if (!$(this).hasClass('item-deleted')) {
			tmpcnt = parseInt($(this).find('.prices p.count span.count').text());
			if (rCart){
				for (i = 0; i < rCart.length; i++){
					if (rCart[i].id == parseInt($(this).find('.id').text()))
						rCart[i].count = tmpcnt;
				}
			}
			//rCartReDraw();
			var tmpsum = parseInt($(this).find('.info big.price span').text()) * tmpcnt;
			sum += tmpsum;
			$(this).find('.prices big.price span').text(tmpsum)
			var tmpsumold = parseInt($(this).find('.info big.price-old span').text()) * tmpcnt;
			if (tmpsumold) {
				sumold += tmpsumold;
			} else {
				sumold += tmpsum;
			}
			$(this).find('.accs .acc').each(function(){
				if ($(this).find('.cb .checkbox:checked').length > 0) {
					sum += parseInt($(this).find('big.price span').text());
					sumold += parseInt($(this).find('big.price span').text());
				}
			})
			cnt = cnt + tmpcnt;
		}
	});
	if (rCart)
		rSave(rCart);
	$('.go-checkout').each(function(){
		var current = $(this).find('.total');
		var cprice = $(this).find('.price big').not('.gray');
		var cpriceold = $(this).find('.gray big');
		if(cnt > 0) {
			$(this).find('.price').show();
			if (cnt == 1) {
				$(current).html('Всего '+cnt+' товар на сумму');
			}
			else
				if (cnt > 3) {
					$(current).html('Всего '+cnt+' товаров на сумму');
				}
				else {
					$(current).html('Всего '+cnt+' товара на сумму');
				}
			$(cprice).html(sum);
			$(cpriceold).html(sumold);
		} else {
			$(current).html('Нет товаров');
			$(this).find('.price').hide();
		}
	});
}

function initRCartReDraw2 () {
	var i = 0;
	$('.catalog-cart2 .item').each(function(){
		i++;
		var tmp = $(this);
		$(tmp).find('.num').text(i);
		var cur = $(this);
		(function(cur) {
			$(tmp).find('.delete a').unbind('click').click(function(){
				$(this).parents('.item').addClass('item-deleted');
				$(this).parents('.item').find('.pic').animate({opacity: 0.5}, 0);
				rDelCart(cur[0].id);
				rCartReDraw2();
				rCartReDraw();
				return false;
			});
			$(tmp).find('.return a').unbind('click').click(function(){
				$(tmp).find('.pic').animate({opacity: 1}, 0);
				$(tmp).removeClass('item-deleted');
				rCartReDraw2();
				rCartReDraw();
				return false;
			});
		})(cur);
		$(tmp).find('.actions p.count input.text').keydown(function(evt){
			var theEvent = evt || window.event;
			var key = theEvent.keyCode || theEvent.which;
			keyChar = String.fromCharCode(key);
			var regex = /\d|[\x60-\x6A]/; 
			if (!regex.test(keyChar)) {
				if((key!=8) && (key!=37) && (key!=39)) {
					return false;
				}
			}
			return true;
		}).keyup(function(){
			if (parseInt($(this).val()) > 0) {
				$(this).change();
			} else if (parseInt($(this).val()) == 0) {
				$(this).parents('.item').addClass('item-deleted');
				$(this).parents('.item').find('.pic').animate({opacity: 0.5}, 0);
				$(this).val(1);
				$(this).parents('.item').find('.prices p.count span.count').text($(this).val());
			}
			rCartReDraw2();
		}).change(function(){
			$(this).parents('.item').find('.prices p.count span.count').text($(this).val());
			rCartReDraw2();
		}).change();

		$(tmp).find('.accs .acc input.checkbox').click(function(){
			rCartReDraw2();
		});
	});
	rCartReDraw2();
}
function checkoutProceed(current) {
	var formname = $(current).parents('.form').attr('id').replace('checkout-','');
	var mask = $(current).parents('.form').attr('id')+'-';
	var noerrors = true;
	var errorFields = new Array();
	var filter = /^([a-zA-Z0-9_.-])+@(([a-zA-Z0-9-])+.)+([a-zA-Z0-9]{2,4})+$/;
	$(current).parents('.form').find(":input.required").each(function(){
		if ($(this).val().toString() === '') {
			if (!$(this).parent('p').hasClass('error')) {
				$(this).parent('p').addClass('error');
				$("<label class='errorlabel'>Не заполнено обязательное поле</label>").insertAfter(this);
			}
			noerrors = false;
		} 
		else {
			if (($(this).hasClass('required-email')) && (!filter.test($(this).val()))) {
				$(this).parent('p').addClass('error').find('.errorlabel').remove();
				$("<label class='errorlabel'>Введите правильный e-mail</label>").insertAfter(this);
				noerrors = false;
			} 
			else {
				$(this).parent('p').removeClass('error').find('.errorlabel').remove();
			}
		}
	});
	if (!noerrors) {
		$(current).parents('.form').find('p.error').eq(0).find(':input').focus();
	} 
	else {
		obj = new Object;
		$(current).parents('.form').find(':input').each(function(){
			var name = $(this).attr('id').replace(mask,'');
			var val = $(this).val();
			if ($(this).is(":checkbox")) {
				obj[name]=$(this).is(":checked");
			} 
			else if ($(this).is(":radio")) {
				if ($(this).is(":checked") && $(this).attr('id').indexOf("addr-")>0) {
					obj['addr'] = val;
				}
			} 
			else {
				obj[name] = val;
			}
		});
		var rUser = eval("("+rGetCookie('user')+")");
		if (!rUser) {
			rUser = new Object();
		}
		if (formname == 'oldbuyer' || formname == 'newbuyer' || formname == 'anonym') {
			rUser.auth = obj;
		}
		else if (formname == 'auto' || formname == 'courier') {
			rUser.delivery = obj;
		}
		else if (formname == 'cash' || formname == 'card' || formname == 'credit' || formname == 'bank') {
			rUser.pay = obj;
		}
		rSetCookie('user', JSON.stringify(rUser));
		window.location = $(current).attr('href');
	}
}

function checkoutFinish(current) {
	var where = $(current).find('.temparea');
	var rUser = eval("(" + rGetCookie('user') + ")");
	var temp = '';
	var rCart = eval(rGetCookie('cart'));
	var sum = 0;
	if (rCart) {
		for (i = 0; i < rCart.length; i++) {
			sum += rCart[i].price * rCart[i].count;
		}
	}
	//console.log(rUser);
	if (rUser.auth.authtype == 'anonym') {
		if (rUser.auth.name) {
			temp += rUser.auth.name + '<br/>';
		}
		temp += rUser.auth.phone;
	}
	else
		if (rUser.auth.authtype == 'newbuyer') {
			if (rUser.auth.name) {
				temp += rUser.auth.name + ' ';
			}
			if (rUser.auth.family) {
				temp += rUser.auth.family;
			}
			temp += '<br/>';
			temp += rUser.auth.phone + '<br/>';
			temp += rUser.auth.email;
		}
	where.append('<p>' + temp + '</p>');
	temp = '';
	if (rUser.delivery.deliverytype == 'courier') {
		// if (sum < 10000) {
			where.append('<p class="h2">Доставка курьером</p><p class="discount-shipping">Стоимость <strong>300 руб.,</strong> завтра в течение дня</p>');
		// } else {
			// where.append('<p class="h2">Доставка курьером</p><p class="discount-shipping"><strong>бесплатно,</strong> завтра в течение дня</p>');
		// }
		// temp += rUser.delivery.city + '<br/>';
		temp += rUser.delivery.addr;
		if (rUser.delivery.comment) {
			temp += '<br/>' + rUser.delivery.comment;
		}
		temp += '<br/><a href="checkout2">Изменить способ доставки</a>'
	}
	else
		if (rUser.delivery.deliverytype == 'auto') {
			where.append('<p class="h2">Забрать самостоятельно</p>');
			temp += rUser.delivery.addr;
			temp += '<br/><a href="checkout2.html">Изменить способ доставки</a>'
		}
	where.append('<p>' + temp + '</p>');


	temp = '';
	if (rUser.pay.paytype == 'cash') {
		where.append('<p class="h2">Оплата наличными</p><p>Самостоятельно в магазине, или курьеру при получении товара. Вы получите товарный чек.</p>');
	}
	else
		if (rUser.pay.paytype == 'card') {
			where.append('<p class="h2">Оплата кредитной картой</p>');
		}
		else
			if (rUser.pay.paytype == 'credit') {
				where.append('<p class="h2">Покупка в кредит</p>');
			}
			else
				if (rUser.pay.paytype == 'bank') {
					where.append('<p class="h2">Оплата по безналичному расчету</p>');
					where.append('<p>Реквизиты:<br/>' + rUser.pay.bankaccount + '</p>');
				}
	if (/*sum < 10000 &&*/ rUser.delivery.deliverytype == 'courier') {
		where.append('<br/><p class="price"><big>' + (sum + 300) + '</big> руб.</p>');
	} else {
		where.append('<br/><p class="price"><big>'+sum+'</big> руб.</p>');
	}
	where.append('<p><a href="checkout3.html">Изменить способ оплаты</a></p>');
}


function checkoutThanks(current) {
	var where = $(current).find('.temparea');
	var rUser = eval("(" + rGetCookie('user') + ")");
	var temp = '';
	var rCart = eval(rGetCookie('cart'));
	var sum = 0;
	if (rCart) {
		for (i = 0; i < rCart.length; i++) {
			sum += rCart[i].price * rCart[i].count;
		}
	}
	if (rUser.auth.authtype == 'anonym') {
		temp += '<div class="checkout-green"><p class="h2">Мы получили ваш заказ.</p><p>В течение часа с вами свяжется наш менеджер и уточнит детали заказа. На ваш адрес <b>'+rUser.auth.mail+'</b> отправлено письмо с информацией о заказе.</p></div>';
	}
	else
		if (rUser.auth.authtype == 'newbuyer') {
			temp += '<div class="checkout-green"><p class="h2">' + rUser.auth.name + ' ' + rUser.auth.family + ', мы получили ваш заказ.</p><p>В течение часа с вами свяжется наш менеджер и уточнит детали заказа.</p></div>';
		}
	// temp += '<p class="h2">Номер заказа — №9595454</p>';
	// temp += '<p>Этот номер вам пригодится, если вы захотите сами позвонить в службу доставки. Вы сможете сделать это по телефону <big>(812) 320-80-80</big></p>';
	where.append(temp);
	temp = '';
	if (rUser.delivery.deliverytype == 'courier') {
		// if (sum < 10000) {
			where.append('<p class="h2">Стоимость доставки - 300 руб., <strong class="gray">завтра в течение дня</strong></p>');
		// } else {
			// where.append('<p class="h2">Бесплатная доставка курьером <strong class="gray">завтра в течение дня</strong></p>');
		// }

		// temp += rUser.delivery.city + '<br/>';
		temp += rUser.delivery.addr;
		if (rUser.delivery.comment) {
			temp += '<br/>' + rUser.delivery.comment;
		}
	}
	else
		if (rUser.delivery.deliverytype == 'auto') {
			where.append('<p class="h2">Забрать самостоятельно</p>');
			temp += rUser.delivery.addr;
		}
	where.append('<p>' + temp + '</p>');
	temp = '';
	if (rUser.pay.paytype == 'cash') {
		where.append('<p class="h2">Оплата наличными</p><p>Курьеру при получении товара. Вы получите товарный чек</p>');
	}
	else
		if (rUser.pay.paytype == 'card') {
			where.append('<p class="h2">Оплата кредитной картой</p>');
		}
		else
			if (rUser.pay.paytype == 'credit') {
				where.append('<p class="h2">Покупка в кредит</p>');
			}
			else
				if (rUser.pay.paytype == 'bank') {
					where.append('<p class="h2">Оплата по безналичному расчету</p>');
					where.append('<p>Реквизиты:<br/>' + rUser.pay.bankaccount + '</p>');
				}
		if (rUser.delivery.deliverytype == 'courier' /*&& sum < 10000*/) {
		where.append('<br/><p class="price"><big>' + (sum + 300) + '</big> руб.</p>');
	} else {
		where.append('<br/><p class="price"><big>'+sum+'</big> руб.</p>');
	}
		rCart = new Array();
		rSetCookie('cart', JSON.stringify(rCart));
}



function rCartReDraw3() {
	var rCart = eval(rGetCookie('cart'));
	if (rCart) {
		$('.your-order').find('.items li').remove();
		var cur =$('.your-order').find('.items ol');
		for (i = 0; i < rCart.length; i++) {
			var tmp = $('<li><span class="name"></span><span class="count"></span><br/><b class="price"></b> руб.</li>');
			tmp.find('.name').text(rCart[i].name);
			tmp.find('.price').text(rCart[i].price);
			if (parseInt(rCart[i].count) > 1) {
				tmp.find('.count').text(' x ' + rCart[i].count);
			}
			//var cur = rCart[i];
			tmp.appendTo(cur);
		};
	}
}


$(document).ready(function() {	
	/*Инициализация ранее заполненных полей*/
	var rUser = eval("(" + rGetCookie('user') + ")");
	if (rUser){
		if (rUser.auth){
			if (rUser.auth.authtype == 'newbuyer'){
				if ((document).getElementById('checkout-newbuyer-name')){
					(document).getElementById('checkout-newbuyer-name').value = rUser.auth.name;
				}
				if ((document).getElementById('checkout-newbuyer-family')){
					(document).getElementById('checkout-newbuyer-family').value = rUser.auth.family;
				}
				if ((document).getElementById('checkout-newbuyer-email')){
					(document).getElementById('checkout-newbuyer-email').value = rUser.auth.email;
				}
				if ((document).getElementById('checkout-newbuyer-phone')){
					(document).getElementById('checkout-newbuyer-phone').value = rUser.auth.phone;
				}
			}	
			else {
				if (rUser.auth.authtype == 'anonym'){
					if ((document).getElementById('checkout-anonym-name')){
						(document).getElementById('checkout-anonym-name').value = rUser.auth.name;
					}
					if ((document).getElementById('checkout-anonym-phone')){
						(document).getElementById('checkout-anonym-phone').value = rUser.auth.phone;
					}
				}
			}
		}
		if (rUser.delivery){
			if (rUser.delivery.deliverytype == 'courier'){
				if ((document).getElementById('checkout-courier-addr')){
					(document).getElementById('checkout-courier-addr').value = rUser.delivery.addr;
				}
				if ((document).getElementById('checkout-courier-comment')){
					(document).getElementById('checkout-courier-comment').value = rUser.delivery.comment;
				}
			}
		}
	}
	/* Дефолтный блок, округленные углы */
	$('.block').append('<i class="tl"></i><i class="tr"></i><i class="bl"></i><i class="br"></i>');

	/* Большой нумерованный список */
	$('.olist ol,.list-4').each(function() {
		var li;
		li = $(this).children('li');
		$(li).each(function() {
			var i = (li.index(this) + 1);
			$(this).append('<var>'+i+'</var>');
		});
	});

	/* Слайдер "товар дня" */
	$(".block-daily-item .switch .slider").jcarousel({
		initCallback: dailySliderInitCallback,
		itemLoadCallback: dailySliderItemLoadCallback,
		scroll: 1,
		buttonNextHTML: null,
		buttonPrevHTML: null,
		wrap: 'circular',
		vertical: true
	});

	/* Слайдер "лучшие цены" */
	if (!($.browser.msie && $.browser.version < 7)) {
		$(window).resize(function(){
			refreshWidth();
		});
		refreshWidth();
	}

	/* Каталог товаров */
	if (($.browser.msie && $.browser.version < 7)) {
		$(".catalog .item").hover(function(){$(this).addClass('item-hover');}, function(){$(this).removeClass('item-hover');});
	}
	$(".catalog").each(function(){
		$(this).find(".item:first").addClass('item-first');
	});

	/* Переключалка брендов */
	$(".producer-block a[rel='switch']").click(function(){
		var sw = $(this).parent('.switch')
		var bl = $(this).parents('.producer-block')
		if ($(sw).hasClass('switch-full')){
			$(sw).removeClass('switch-full').addClass('switch-collapsed');
			$(this).text("Сокращённый список");
			$(bl).find(".producer-list-collapsed").stop().slideDown(200);
		} else if ($(sw).hasClass('switch-collapsed')) {
			$(sw).removeClass('switch-collapsed').addClass('switch-full');
			$(this).text("Полный список");
			$(bl).find(".producer-list-collapsed").stop().slideUp(200);
		}
		return false;
	});


	/* Переключалка */
	$(".choose-laptop a[rel='switch']").click(function(){
		var sw = $(this).parent('.switch')
		var bl = $(this).parents('.choose-laptop')
		var attr = $(this).attr("href").replace(/^.*#(.*)/, "$1");
		if ($(sw).hasClass('switch-full')){
			$(sw).removeClass('switch-full').addClass('switch-collapsed');
			$('#'+attr).slideDown(200);
		} else if ($(sw).hasClass('switch-collapsed')) {
			$(sw).removeClass('switch-collapsed').addClass('switch-full');
			$('#'+attr).stop().slideUp(200);
		}
		return false;
	});

	/* Переключалка  отзывы */
	$(".more-reviews a[rel='switch']").click(function(){
		var sw = $(this).parent('.switch')
		var bl = $(this).parents('.reviews-full-block')
		if ($(sw).hasClass('switch-full')){
			$(sw).removeClass('switch-full').addClass('switch-collapsed');
			$(this).text("Свернуть отзывы");
			$(bl).find(".reviews-collapsed").stop().slideDown(600);
		} else if ($(sw).hasClass('switch-collapsed')) {
			$(sw).removeClass('switch-collapsed').addClass('switch-full');
			$(this).text("Остальные отзывы");
			$(bl).find(".reviews-collapsed").stop().slideUp(600);
		}
		return false;
	});


	/* Табы */
	$(".tabs .h2 a").not('.all').click(function(){
		var where = $(this).attr("href").replace("#","");
		$(this).parents('.tabs').find('li').removeClass('active');
		$(this).parents('.h2').addClass('active');
		$(this).parents('.tabs').siblings('.tab').addClass('tab-hidden');
		$(this).parents('.tabs').siblings('.tab-'+where).removeClass('tab-hidden');
		return false;
	});

	$(".acc-nav li a").click(function(){
		var where = $(this).attr("href").replace("#","");
		var acc = $(this).parents('.accessories');
		$(this).parents('.acc-nav').find('li').removeClass('active');
		$(this).parents('li').addClass('active');
		$(this).parents('.accessories').find('.acc-tab').addClass('acc-tab-hidden');
		$(this).parents('.accessories').find('.acc-tab-'+where).removeClass('acc-tab-hidden');
		if ($(acc).find('.acc-tab-'+where).hasClass('best-items')) {
			$(acc).addClass('accessories-white');
		} else {
			$(acc).removeClass('accessories-white');
		}
		refreshWidth();
		return false;
	});

	/* Табы */
	$(".search-fast input.text").keyup(function(){
		var current = $(this);
		var s = $(current).val().trim();
		$('.catalog-list').each(function(){
			$('.item').each(function(){
				$(this).find('.h2,li').each(function(){
					if ($(this).is(":Contains('" + s + "')")) {
						$(this).addClass("yes").removeClass("no");
					}
					else {
						$(this).addClass("no").removeClass("yes");
					}
				});
				if ($(this).find('.h2,li').filter('.yes').length == 0) {
					$(this).addClass("no").removeClass("yes");
				} else {
					$(this).addClass("yes").removeClass("no");
				}
			});
			if ($(this).find('.item').filter('.yes').length == 0) {
				$(this).addClass("no").removeClass("yes");
				$(this).next('.link-up').addClass("link-no").removeClass("link-yes");
			} else {
				$(this).addClass("yes").removeClass("no");
				$(this).next('.link-up').addClass("link-yes").removeClass("link-no");
			}
			if ($(this).siblings('.catalog-list').andSelf().filter('.yes').length == 0) {
				$(this).siblings('.catalog-filter-none').show();
			} else {
				$(this).siblings('.catalog-filter-none').hide();
			}
		});
	})


	/* Слайдер мелкие картинки в товаре */
	$(".block-item-pics ul").jcarousel({
		scroll: 1
	});

	$('.rating a').hover(function(){
		var attr = $(this).attr("href").replace(/^.*#(.*)/, "$1");
		$(this).parents('.rating').addClass('h-rating'+attr);
		$(this).parents('.rating').siblings('.txt').text($(this).attr('title'));
	},function(){
		var attr = $(this).attr("href").replace(/^.*#(.*)/, "$1");
		$(this).parents('.rating').removeClass('h-rating'+attr);
		$(this).parents('.rating').siblings('.txt').text('');
	}).click(function(){
		var attr = $(this).attr("href").replace(/^.*#(.*)/, "$1");
		$(this).parents('.rating').removeClass('rating1').removeClass('rating2').removeClass('rating3').removeClass('rating4').removeClass('rating5').addClass('rating'+attr);
		if ($(this).parents('.rating').hasClass('rating-ajax')){
			/* если есть класс .rating-ajax - то тут аяском на сервак улетает новый рейтинг - переменная attr */
			alert('Новый рейтинг - ' + $(this).attr('title'));
		} else {
			/* если класса нету - то рузальтат хранится в input:hidden */
			$(this).parents('.rating').siblings('.rating-hidden').val(attr);
		}
		return false;
	});


	initPriceAcc();
	refreshPriceAcc();

	/* === Инициализация попапов === */
	var h = $('.container').height();
	if (h <= $('body').height()) {
		h = $('body').height();
	}
	$('.overlay').animate({
		opacity: 0
	}, 1);
	$('.overlay').height(h);
	var pre = '<div class="l"><i class="bg"></i><i class="p24"></i></div><div class="c"></div><div class="r"><i class="bg"></i><i class="p24"></i></div>'
	$(".popup").each(function(){
		$(this).wrapInner('<div class="mid"></div>').prepend('<div class="top"></div>').append('<div class="bottom"></div>');
		$(this).find(".top,.bottom").append(pre);
	});

	/* === Клик по линке на попап === */
	$("a[rel='popup']").unbind('click').click(function() {
		var attr = $(this).attr("href").replace(/^.*#(.*)/, "$1");
		showPopup($(this),attr);
		if (!($.browser.msie && $.browser.version < 7)) {
			return false;
		}
	});
	/* === Клик по линке на попап === */
	$("a[rel='tooltip']").unbind('click').click(function() {
		var attr = $(this).attr("href").replace(/^.*#(.*)/, "$1");
		showTooltip($(this),attr);
		return false;
	});

	/* === Клик линке на закрыть === */
	$("a[rel='popup-close']").unbind('click').click(function() {
		hidePopup($(this).parents(".popup"));
		return false;
	});
	$("a[rel='tooltip-close']").unbind('click').click(function() {
		hideTooltip($(this).parents(".tooltip"));
		return false;
	});
	/* === по клику за областью окно выбора тегов - прячем его === */
	$("body").click(function(e) {
		var $target = $(e.target);
		if ($target.parents(".tooltip").length == 0) {
			$(".tooltip").hide();
		}
	});

	/* === Клик линке "развернуть адреса" === */
	$("a[rel='shops-show']").unbind('click').click(function() {
		$(this).parents(".shops-p").siblings(".shops-hidden").slideToggle(300);
		return false;
	});

	/* === Клик линке "техн. характеристики" === */
	$("a[rel='tech-link']").unbind('click').click(function() {
		$("#tech").find(".tabs a[href='#tech']").click();
	});

	rCalc();
	rCartReDraw();
	initRCartReDraw2();

	/* Переключалка в корзине */
	$(".more-link a[rel='switch'],.go-checkout a[rel='switch']").click(function(){
		var where = $(this).attr("href").replace(/^.*#(.*)/, "$1");
		var sw = $('#'+where).find('.more-link');
		var st = $('#'+where);
		if ($(sw).hasClass('more-link-full')){
			$(sw).removeClass('more-link-full').addClass('more-link-collapsed');
			$(sw).find('a').text($(sw).find('a').text().replace('Подробнее','Кратко'));
			$(st).find(".collapsed").stop().slideDown(200);
		} else if ($(sw).hasClass('more-link-collapsed')) {
			$(sw).removeClass('more-link-collapsed').addClass('more-link-full');
			$(sw).find('a').text($(sw).find('a').text().replace('Кратко','Подробнее'));
			$(st).find(".collapsed").stop().slideUp(200);
		}
		if (!$(this).parents('p').hasClass('questions')) {
			return false;
		}
	});

	/* Переключалка в скидках */
	$(".discount-show").click(function(){
		$('.discount-block').slideDown(300,function(){
			$(this).removeClass('discount-block-hidden');
		});
		$('.discount-show').slideUp(200,function(){
			$(this).addClass('discount-show-hidden');
		});
		return false;
	});
	$(".discount-hide").click(function(){
		$('.discount-block').slideUp(300,function(){
			$(this).addClass('discount-block-hidden');
		});
		$('.discount-show').slideDown(200,function(){
			$(this).removeClass('discount-show-hidden');
		});
		return false;
	});

	/* купить в кредит */
	$(".buy-credit label").unbind('change').change(function() {
		if ($(this).find(':checked').length > 0) {
			$(this).parent().addClass('buy-credit-checked');
		}
		else {
			$(this).parent().removeClass('buy-credit-checked');
		}
		return false;
	}).unbind('click').click(function() {
		$(this).change();
		//return false;
	}).change();

	/* навигация оформления заказа */
	$('.checkout-nav li').each(function(){
		if (!$(this).is(':last-child')) {
			$(this).append('<var></var>');
	}
	});

	$('.checkout-tab a[rel="switch"],.checkout-thanks a[rel="switch"]').click(function(){
		var attr = $(this).attr("href").replace(/^.*#(.*)/, "$1");
		$('.switch-one').each(function(){
			$(this).parents('.checkout-tab').find('.h2').removeClass('h2-active');
			$(this).addClass('switch-hidden');
		});
		if ($('#'+attr).hasClass('switch-hidden')) {
			if ($(this).parent().hasClass('h2')) {
				$(this).parent().addClass('h2-active');
			}
			$('#'+attr).removeClass('switch-hidden');
		} else {
			if ($(this).parent().hasClass('h2')) {
				$(this).parent().removeClass('h2-active');
			}
			$('#'+attr).addClass('switch-hidden');
		}
		reloadPage();
		return false;
	});
	
	$('.checkout-tab a[rel="switch-map"]').click(function(){
		var attr = $(this).attr("href").replace(/^.*#(.*)/, "$1");
		if ($('#'+attr).hasClass('switch-hidden')) {
			$('#'+attr).removeClass('switch-hidden');
		} else {
			$('#'+attr).addClass('switch-hidden');
		}
		reloadPage();
		return false;
	});

	$("#slider-price").each(function(){
		$(this).slider({
			range: true,
			min: 0,
			max: 60000,
			values: [0, 45000],
			slide: function(event, ui){
				$("#slider-price-from").val(ui.values[0]);
				$("#slider-price-to").val(ui.values[1]);
			}
		});
		$("#slider-price-from").val($("#slider-price").slider("values", 0));
		$("#slider-price-to").val($("#slider-price").slider("values", 1));
	});


	/* Cookie в корзине */
	$('.checkout-tab .form').find(':input').keypress(function(e) {
			/* перехватываем нажатие на Enter */
			if (e.which == 13) { return false; }
		}).keyup(function(e) {
			if (e.which == 13) {
				checkoutProceed($(this).parents('form').find("a[rel='submit-cookie']"));
			}
		});
	$("a[rel='submit-cookie']").click(function(){
		checkoutProceed(this);
		return false;
	});

	$('.your-order').each(function(){
		rCalc();
		rCartReDraw3();
	});
	$('.checkout-finish').each(function(){
		checkoutFinish(this);
	});
	$('.checkout-thanks').each(function(){
		checkoutThanks(this);
	});
	$(".block-item-pics ul li a,.item-info-left .pic a").fancybox({
		'transitionIn'	:	'fade',
		'transitionOut'	:	'fade',
		'speedIn'		:	300,
		'speedOut'		:	200,
		'overlayShow'	:	true,
		'hideOnOverlayClick':	true,
		'showNavArrows'	:	true,
		'overlayOpacity':	0.4,
		'overlayColor'	:	'#000',
		'titleShow'		: true,
		'titlePosition'	: 'over'
	});
		$(".iframe,.add a").fancybox(
			{
				'content' : '<div class="product-add-complete">Товар добавлен в корзину!</div>',
				'transitionIn'	:	'fade',
		'transitionOut'	:	'fade',
		'overlayShow'	:	true,
		'hideOnOverlayClick':	true,
		'speedIn'		:	200,
		'speedOut'		:	200,
				'width'    : 240,
				'height'   : 'auto',
				'autoDimensions' : false,
				'centerOnScroll' : true,
				'padding'  : 20,
				'scrolling' : 'no',
				'onComplete'   :  function (){setTimeout("closeFancy()",2000);}
			});
});
