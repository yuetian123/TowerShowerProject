using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
public class AwardItem : MonoBehaviour
{
    public string name;
    public Sprite image;
    public string info;

    private void OnTriggerEnter(Collider other)
    {
        if (name == "")
        {
            GameSystem.Instance.money++;
            AwardInfo.Instance.SetInfo("获得一个纪念币",Color.yellow);
            this.gameObject.SetActive(false);
            return;
        }
        Bag.Instance.AddItem(new Item() { name = this.name,image = this.image ,info = this.info },"恭喜获得" + this.name +"，已放入背包。");
        this.gameObject.SetActive(false);
    }
    private void Update()
    {
        transform.Rotate(0,1,0);
    }
}
