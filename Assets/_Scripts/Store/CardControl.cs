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
        //���˷��������ޡ����Ϸ��š�ͨ�ݹǵ񡢱���������ƤӰ���ϱ����ǻ�
            new Item() { name = "��������",image = ItemLibrary.Instance.sprs[0] ,info = "ֽ�������й���ͳ��ֽ�ƹ���Ʒ,�����������ã���ˮ��ǿ��ר��ֽΪ��Ҫԭ�ϣ�����ճ�������У���Ⱦ��20������ֹ������������ӹ��������ɡ�" },
            new Item() { name = "���Ϸ���", image = ItemLibrary.Instance.sprs[1] ,info = "���Ϸ�����600����Ļ�����50������ֹ�����,���ŵĹ��շǳ����ӣ�ѡ�ϡ���̥����˿����������ĥ�����ӣ����ָ����Ĺ��򣬲�����һ�����ˣ�����Ӱ������Ч������Ʒ�ʼ��ߡ�" },
            new Item() { name = "���˷���������", image = ItemLibrary.Instance.sprs[2] ,info = "���˷����������ڡ��������ˡ���ͳ���յĻ����ϴ�����Ƴ�������Ʒ���й��űȡ������ޣ���������£�������������������Ũ����й��硣" },
            new Item() { name = "ͨ�ݹǵ�", image = ItemLibrary.Instance.sprs[3] ,info = "�ǵ����ڶ����ͷ�Ͻ��е�̵��������ֳƹǿ̡�һ��ѡ��ţ�����յ�С�ȹǺ���ǣ����ǰ�����ڹ����ϻ�����ñʹ��ճ����ͼ������õ��λ�á�֮���þ⡢���ӵȹ��ߵ�̳������������Ϊ���������Բ����Ρ��̵ķ�ʽ��ɾ�ϸ��̣���Ϊ���������ʹ��Ʒϸ��׼ȷ�������ࡢ���������������д�ĥ���׹⴦����ȥ�����ϱ���ϸ΢�ĵ�̺ۼ���" },
            new Item() { name = "�ϱ����ǻ�", image = ItemLibrary.Instance.sprs[4] ,info = "�ǻ���һ�ִ�ͳ����ֹ��գ������������������໭���ɹۿ�ʳ������׳ơ������˶����������Ǳ��������ǵ�Ӱ��������Ϊƽ���ǻ��������ǻ����֣��ǵص�����仭�֣��ľ���ɫ�Ľ��������������ϰ���ϲ���Ĺ���ʳƷ��" }
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
