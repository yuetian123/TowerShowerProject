using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.EventSystems;
public class BagPlace : MonoBehaviour,IComparable<BagPlace>,IPointerClickHandler
{
    public int index;
    public bool isNull = true;
    public Text text;
    public Image logo;
    public string info;

    private void Awake()
    {
        Bag.Instance.places.Add(this);
        isNull = true;
    }

    public void AddItem(Item item)
    {
        text.text = item.name;
        logo.sprite = item.image;
        info = item.info;
    }

    public int CompareTo(BagPlace other)
    {
        if (this.index > other.index) return 1;
        else return -1;
    }

    public void OnPointerClick(PointerEventData eventData)
    {
        Bag.Instance.info.text = info;
    }
}
