using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class AwardInfo : BaseMonoClass<AwardInfo>
{
    private Text text;

    private float isTimer;


    protected override void Awake()
    {
        base.Awake();
        text = GetComponent<Text>();
    }
    private void Update()
    {
        if (isTimer > 0)
        {
            isTimer -= Time.deltaTime;
        }else if (text.text != "")
        {
            text.text = "";
        }
    }
    public void SetInfo(string info,Color color)
    {
        text.color = color;
        text.text = info;
        isTimer = 5;
    }
    public void BuyWin(bool flag)
    {

        if (flag)
        {
            text.color = Color.green;
            text.text = "购买成功!";
            isTimer = 5;
        }
        else
        {
            text.color = Color.red;
            text.text = "纪念币不足!";
            isTimer = 5;
        }

    }
    public void SetInfo(NPCType type)
    {
        if (text.color == Color.green || text.color == Color.red)
        {
            text.color = Color.yellow;
        }
        string str = "";
        if (type == NPCType.Cat)
        {
            str = "恭喜获得奖励(纪念币 * 5)";
        }else if(type == NPCType.Dog)
        {
            str = "恭喜获得访览文庙奖励(纪念币 * 5)";
        }
        else if (type == NPCType.Squirrel)
        {
            str = "恭喜获得访览佑胜教寺的奖励(纪念币 * 5)";
        }
        else if (type == NPCType.Bird)
        {
            str = "恭喜获得访览燃灯塔的奖励(纪念币 * 5)";
        }
        else if (type == NPCType.Shh)
        {
            str = "恭喜获得访览紫清宫的奖励(纪念币 * 5)";
        }
        else
        {
            text.color = Color.green;
            str = "已经领取过此处奖励，可以继续游玩探索";
        }
        text.text = str;
        isTimer = 5;
    }
    public void Repeat()
    {
        SetInfo(NPCType.NULL);
    }
}
