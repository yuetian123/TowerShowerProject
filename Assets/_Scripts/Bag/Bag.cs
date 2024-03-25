using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class Bag : BaseMonoClass<Bag>
{
    public Text info;//介绍信息

    public List<BagPlace> places;//背包的格格位置

    protected override void Awake()
    {
        base.Awake();
        places = new List<BagPlace>();
    }

    private void Start()
    {
        places.Sort();
        gameObject.SetActive(false) ;
    }
    public void AddItem(Item item)
    {
        foreach (var place in places)
        {
            if (place.isNull)
            {
                place.AddItem(item);
                place.isNull = false;
                AwardInfo.Instance.BuyWin(true);
                return;
            }
        }
    }
    public void AddItem(Item item,string info)
    {
        foreach (var place in places)
        {
            if (place.isNull)
            {
                place.AddItem(item);
                place.isNull = false;
                AwardInfo.Instance.SetInfo(info,Color.green);
                return;
            }
        }
    }
}
