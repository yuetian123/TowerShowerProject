using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class Speak : BaseMonoClass<Speak>
{
    public DisplayText value;
    private Canvas c;
    private Player player;

    private NPCType currentNPC;
    protected override void Awake()
    {
        base.Awake();
        value = transform.Find("Speak/Value").GetComponent<DisplayText>();
        c = GetComponent<Canvas>();
        gameObject.SetActive(false);
        player = GameObject.FindGameObjectWithTag("Player").GetComponent<Player>() ;
        
    }
    //private void Update()
    //{
    //    if (Input.GetKeyDown(KeyCode.Space))
    //    {
    //        value.End();
    //    }
    //}
    public void StartSpeak(NPCType state)
    {
        currentNPC = state;
        if (state == NPCType.Cat)
        {
            value.SetValue("通州“三庙一塔”景区是国内唯一的三教合一建筑群，占地约12000平方米。“三庙一塔”分别指的是：文庙、佑胜教寺、紫清宫及燃灯佛舍利塔。儒、佛、道三教在这里互为紧邻而又相互独立，这三座独立存在的庙宇，近距离呈“品”字形布局，和谐共存了400余年。今人概括简称之为“三教庙”，成为北京“人文奥运”六大景区之一——通州运河文化景区的重要组成部分。 　　“三庙一塔”景区是国内唯一的三教合一建筑群，也称“三教庙”，是三座独立的庙宇，占地约12000平方米。“三庙一塔”分别指的是：文庙(儒家学府，又称学宫)、紫清宫(俗称红孩儿庙)及燃灯塔及其附建的佑胜教寺。 　　儒、释、道三教在这里互为紧邻而又相互独立，这三座独立存在的庙宇，近距离呈“品”字形布局。据考，燃灯塔始建于北周，而紫清宫建于明代中期，三教和谐共存了400余年。今人概括简称之为“三教庙”，成为北京“人文奥运”六大景区之一——通州运河文化景区的重要组成部分。");
            c.Show();
            Views.Instance.camera[(int)state].SetActive(true);
        }else if (state == NPCType.Dog)
        {
            value.SetValue("通州文庙，始建于元代大德二年（1298年），比位于东城区国子监旁的孔庙还要年长4岁，是北京地区现存最古老的孔庙。燃灯塔与文庙相映相依，成为城市副中心地标性古建。文庙戟门前，有泮桥、泮池。其中泮桥由三座小石桥构成，自元大德二年始建起，历经元、明、清多次扩建。泮桥下是泮池。在古代，新晋的秀才进入县学“深造”，都要从泮桥经过去参拜孔子。经过修缮，目前已恢复了文庙历史格局。其中，变化最大的是泮桥——因文物保护需要，桥上此前用玻璃罩罩住，游客只能透着玻璃观察泮桥的基座。这次修缮特意在基座上恢复了泮桥，对泮池进行了重修，恢复蓄水功能，实现桥上通行。");
            c.Show();
            Views.Instance.camera[(int)state].SetActive(true);
        }else if (state == NPCType.Squirrel)
        {
            value.SetValue("佑胜教寺俗称“塔庵”，位于燃灯塔旁边。一进院落，便看到一棵树龄350余年的国槐，古树三米以上主干已折断，虽经历战火和自然灾害，但依然挺拔。古树是大运河记忆的一部分，古树保护是运河文脉传承中的一道独特风景。");
            c.Show();
            Views.Instance.camera[(int)state].SetActive(true);
        }
        else if (state == NPCType.Bird)
        {
            value.SetValue("“一枝塔影认通州”说的就是燃灯塔。燃灯塔始建于北周年间，历代多次重修，是北京地区建造年代最早、最高大的佛塔之一。现存古塔为辽塔形制，塔高56米，密檐式实心砖木结构，塔檐悬挂2248枚铜铃，微风吹过，古塔“层层高耸接青云，朗朗铃音空里鸣。”燃灯塔距今已有1400多年的历史，是大运河千年漕运的航标，一直矗立于此，默默看着历史的变迁。");
            c.Show();
            Views.Instance.camera[(int)state].SetActive(true);
        }
        else if (state == NPCType.Shh)
        {
            value.SetValue("紫清宫俗称“红孩儿庙”，始建于明代中期，现为清代建筑。现存山门、正殿和东、西配殿各一座。正殿东山墙正中有铭文记载清光绪年间重修紫清宫的始末。");
            c.Show();
            Views.Instance.camera[(int)state].SetActive(true);
        }
        player.OverUsing(0);
    }
    public void Award()//奖励方法
    {
        //判断只能进行一次奖励
        if (MapManager.Instance.state[(int)currentNPC] == 0)
        {
            MapManager.Instance.Open((int)currentNPC);
            AwardInfo.Instance.SetInfo(currentNPC);
            GameSystem.Instance.money += 5;
            GameSystem.Instance.plan.value++;
        }
        else
        {
            AwardInfo.Instance.Repeat();
        }
    }
}

