using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class GameSystem : BaseMonoClass<GameSystem>
{
    public Slider plan;
    public bool isWin;
    public int money;
    protected override void Awake()
    {
        base.Awake();
        money = 0;
        
    }

    public void Update()
    {
        if (Input.GetKeyDown(KeyCode.LeftAlt))//按alt键隐藏或显示鼠标
        {
            Cursor.visible = !Cursor.visible;
        }

        if (plan.value >= 5 && !isWin)
        {
            isWin = true;
            Win();
        }

    }
    public void Win()//玩家主线已经探索完成
    {
        AwardInfo.Instance.SetInfo("恭喜您已成功打卡三庙一塔,奖励10个纪念币",Color.yellow);
        GameSystem.Instance.money += 10;
        //MapManager.Instance.line.SetActive(false);
    }
    
}
