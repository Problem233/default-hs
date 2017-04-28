package io.github.problem233.default

import language.postfixOps
import scala.util.Random

/**
  * @author Problem233
  */
package object fun {

  def simpleFun1(text: String) = {
    @annotation.tailrec
    def fun(str: String, text: String): String = {
      if (text == "") str
      else fun(
        if (text.length == 1) str + text else str + text + " ",
        text.substring(1)
      )
    }
    fun("", text)
  }

  def fakeSoundWaves(length: Int) = (1 to length).foreach(_ => {
    println((1 to 30).map(_ => {
      var str = " " * 100
      while (str.length > 40) str = "=" * (Integer.MAX_VALUE / Random.nextInt)
      str
    }).foldLeft("")(_ + "\n" + _))
    Thread.sleep(200)
  })

  // Scala古风诗人
  // from: http://www.jianshu.com/p/f893291674ca
  // 可拓展词库与句型，多奇葩句

  lazy val DEFAULT_TWO_CHARS_WORDS = Vector(
    "朱砂", "天下", "杀伐", "人家", "韶华", "风华", "繁华", "血染", "墨染", "白衣",
    "素衣", "嫁衣", "倾城", "孤城", "空城", "旧城", "旧人", "伊人", "心疼", "春风",
    "古琴", "无情", "迷离", "奈何", "断弦", "焚尽", "散乱", "陌路", "乱世", "笑靥",
    "浅笑", "明眸", "轻叹", "烟火", "一生", "三生", "浮生", "桃花", "梨花", "落花",
    "烟花", "离殇", "情殇", "爱殇", "剑殇", "灼伤", "仓皇", "匆忙", "陌上", "清商",
    "焚香", "墨香", "微凉", "断肠", "痴狂", "凄凉", "黄梁", "未央", "成双", "无恙",
    "虚妄", "凝霜", "洛阳", "长安", "江南", "忘川", "千年", "纸伞", "烟雨", "回眸",
    "公子", "红尘", "红颜", "红衣", "红豆", "红线", "青丝", "青史", "青冢", "白发",
    "白首", "白骨", "黄土", "黄泉", "碧落", "紫陌")
  lazy val DEFAULT_FOUR_CHARS_WORDS = Vector(
    "情深缘浅", "情深不寿", "莫失莫忘", "阴阳相隔", "如花美眷", "似水流年", "眉目如画",
    "曲终人散", "繁华落尽", "不诉离殇", "一世长安")
  lazy val DEFAULT_SENTENCE_MODELS = Vector(
    "*，*，*了*。", "^，^，不过是一场^。", "你说^，我说^，最后不过^。", "*，*，许我一场^。",
    "一_一_一*，半_半_半*。", "你说^^，后来^^。", "^，^，终不敌^。")

  //不会取到max
  private def random(max: Int): Int = {
    require(max != 0)
    Random.nextInt(max)
  }

  private def r4(list: Vector[String]) = list(random(list.length))

  private def r2(list: Vector[String]) = list(random(list.length))

  private def r1(list: Vector[String]) = list(random(list.length)).toCharArray.apply(random(2)).toString

  def getSentenceA(model: String, twoCharsWords: Vector[String], fourCharsWords: Vector[String]): String = {
    model.toCharArray.map {
      case '^' => r4(fourCharsWords)
      case '*' => r2(twoCharsWords)
      case '_' => r1(twoCharsWords)
      case x => x.toString
    }.foldLeft("")((a, b) => a + b)
  }

  def getSentenceA(model: String): String = getSentenceA(model, DEFAULT_TWO_CHARS_WORDS, DEFAULT_FOUR_CHARS_WORDS)

  def getSentenceA: String = getSentenceA(DEFAULT_SENTENCE_MODELS(random(DEFAULT_SENTENCE_MODELS.length)))

  def scalaPoetA() = while (true) {
    println(getSentenceA)
    Thread.sleep(500)
  }

  // Scala宋词词人
  // from: http://mp.weixin.qq.com/s?__biz=MzAxMTI5MDc0Nw==&mid=2722796480&idx=1&sn=0783611cff4872aa10aa4f98c9a925ca&scene=23&srcid=0416GsQYz1uDKNWO1JVyTUbZ
  // 可拓展词库，仍然多奇葩句

  lazy val DEFAULT_PASSWORD_LIST = Vector(
    "空", "东风", "何处", "人间", "风流", "归去", "春风", "西风", "归来", "江南",
    "相思", "梅花", "千里", "回首", "明月", "多少", "如今", "阑干", "年年", "万里",
    "一笑", "黄昏", "当年", "天涯", "相逢", "芳草", "尊前", "一枝", "风雨", "流水",
    "依旧", "风吹", "多月", "多情", "故人", "当时", "无人", "斜阳", "不知", "不见",
    "深处", "时节", "平生", "凄凉", "春色", "匆匆", "功名", "一点", "无限", "今日",
    "天上", "杨柳", "西湖", "桃花", "扁舟", "消息", "憔悴", "何时", "芙蓉", "神仙",
    "一片", "桃李", "人生", "十分", "心事", "黄花", "一声", "佳人", "长安", "东君",
    "断肠", "而今", "鸳鸯", "为谁", "十年", "去年", "少年", "海棠", "寂寞", "无情",
    "不是", "时候", "肠断", "富贵", "蓬莱", "昨夜", "行人", "今夜", "谁知", "不似",
    "江上", "悠悠", "几度", "青山", "何时", "天气", "惟有", "一曲", "月明", "往事"
  )

  //会取到min，不会取到max
  @annotation.tailrec
  private def random(min: Int, max: Int): Int = {
    require(max != 0)
    val rand = random(max)
    if (rand < min) random(min, max) else rand
  }

  def getSentenceB(length: Int, passwordList: Vector[String]): String = {
    @annotation.tailrec
    def getSentenceB(length: Int, sentence: String): String = {
      if (length == 0) sentence
      else getSentenceB(length - 1, sentence + passwordList(random(passwordList.length)))
    }

    getSentenceB(length, "")
  }

  def getSentenceB(length: Int): String = getSentenceB(length, DEFAULT_PASSWORD_LIST)

  @annotation.tailrec
  def getSentenceB: String = {
    val sentence = getSentenceB(random(2, 5))
    if (sentence.length == 8) getSentenceB else sentence
  }

  def getSentenceB(password: Vector[Int], passwordList: Vector[String]): String = password.foldLeft("")((a, b) => a + passwordList(b))

  def getSentenceB(password: Vector[Int]): String = getSentenceB(password, DEFAULT_PASSWORD_LIST)

  def scalaPoetB(length: Int) {
    @annotation.tailrec
    def scalaPoetB(length: Int, poem: String): Unit = length match {
      case 0 => println(poem)
      case 1 => scalaPoetB(length - 1, poem + getSentenceB)
      case _ => scalaPoetB(length - 1, poem + getSentenceB + "\n")
    }

    scalaPoetB(length, "")
  }

  // 神之选择器
  // 让你从选择困难症中解脱出来
  def chooser(choices: Int, acc: Int = 10000) =
    (((1 to acc) map (_ => (Random.nextInt % choices).abs + 1) groupBy identity toVector) // 括号防止 postfix operator 被误解
      sortWith (_._2.length > _._2.length)).head._1

}
