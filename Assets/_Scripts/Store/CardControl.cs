using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class CardControl : BaseMonoClass<CardControl>
{
    public RectTransform cav;
    public List<Card> cards;
    public Sprite gen;
    public Sprite current;
    private Vector2 target;
    private int currentIndex = 2;
    private List<Item> items;
    protected override void Awake()
    {
        base.Awake();
        cards = new List<Card>();
        target = Vector2.one;
        items = new List<Item> { 
        //唐人坊绢人娃娃、熊氏珐琅、通州骨雕、北京翻花、皮影、老北京糖画
            new Item() { name = "北京翻花",image = ItemLibrary.Instance.sprs[0] ,info = "纸翻花是中国传统的纸制工艺品,它采用拉力好，吸水性强的专用纸为主要原料，经过粘贴，凿切，罩染等20多道（手工）工序生产加工制作而成。" },
            new Item() { name = "熊氏珐琅", image = ItemLibrary.Instance.sprs[1] ,info = "熊氏珐琅有600年的文化沉淀，50多道纯手工工序,珐琅的工艺非常复杂，选料、制胎、掐丝、点蓝、打磨、焊接，各种各样的工序，不管哪一步错了，都会影响整体效果，废品率极高。" },
            new Item() { name = "唐人坊绢人娃娃", image = ItemLibrary.Instance.sprs[2] ,info = "唐人坊绢人娃娃在“北京绢人”传统技艺的基础上创新设计出的衍生品“中国芭比”唐娃娃，做工巧妙精致，造型栩栩如生，具有浓厚的中国风。" },
            new Item() { name = "通州骨雕", image = ItemLibrary.Instance.sprs[3] ,info = "骨雕是在动物骨头上进行雕刻的艺术，又称骨刻。一般选用牛、骆驼的小腿骨和腕骨，雕刻前，先在骨料上画活，即用笔勾勒出设计图案并标好雕刻位置。之后用锯、凿子等工具雕刻出大概轮廓，称为“凿活”。再以铲、刮、刻的方式完成精细雕刻，称为“铲活”，以使作品细节准确、表面光洁、整体形象传神。最后进行打磨、抛光处理，以去除骨料表面细微的雕刻痕迹。" },
            new Item() { name = "老北京糖画", image = ItemLibrary.Instance.sprs[4] ,info = "糖画是一种传统民间手工艺，以糖作画。它亦糖亦画，可观可食。民间俗称“倒糖人儿”、“倒糖饼儿”或“糖灯影儿”，分为平面糖画与立体糖画两种，是地道的民间画种，颇具特色的街市艺术，备受老百姓喜爱的工艺食品。" }
        };
    }
    private void Update()
    {
        if (target != Vector2.one)
        {
            cav.anchoredPosition = Vector2.Lerp(cav.anchoredPosition,target,0.05f);
            if (Vector2.Distance(cav.anchoredPosition,target) < 0.2f)
            {
                cav.anchoredPosition = target;
                target = Vector2.one;
            }
        }
    }
    public void ChangeCard(int index)
    {
        
        foreach (var item in cards)
        {
            if (item.index == index)
            {
                item.transform.localScale = new Vector3(1,1,1);
                item.GetComponent<Image>().color = new Color(1, 1, 1, 1);
                continue;
            }
            item.transform.localScale = new Vector3(0.9f, 0.9f, 0.9f);
            item.GetComponent<Image>().color = new Color(1,1,1,0.66f);
        }
        target = new Vector2((index - 2) * -410,0);
        currentIndex = index;
    }
    public void Left()
    {
        currentIndex--;
        if (currentIndex < 0)
        {
            currentIndex = 0;
            return;
        }
        ChangeCard(currentIndex);
    }
    public void Right()
    {
        currentIndex++;
        if (currentIndex > 4)
        {
            currentIndex = 4;
            return;
        }
        ChangeCard(currentIndex);
    }
    public void Buy()
    {
        if ((currentIndex == 0 || currentIndex == 4) && GameSystem.Instance.money >= 5)
        {
            Bag.Instance.AddItem(items[currentIndex]);
            GameSystem.Instance.money -= 5;
            transform.parent.gameObject.SetActive(false);
            return;
        }
        if (GameSystem.Instance.money >= 10)
        {
            Bag.Instance.AddItem(items[currentIndex]);
            GameSystem.Instance.money -= 10;
            transform.parent.gameObject.SetActive(false);
            return;
        }
        AwardInfo.Instance.BuyWin(false);
    }
}
