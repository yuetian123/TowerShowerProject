using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class DisplayText : MonoBehaviour
{
    public GameObject item;

    private float count;
    private float speed = 6;
    private Text text;
    private string tempText;
    private bool flag;

    private void Awake()
    {
        text = GetComponent<Text>();
        tempText = text.text;
    }
    private void OnEnable()
    {
        count = 0;
        flag = false;
    }
    public void SetValue(string str)
    {
        tempText = str;
    }
    private void Update()
    {
        if (Input.GetKeyDown(KeyCode.Space))
        {
            count = tempText.Length;
        }
        if (Input.GetMouseButtonDown(0))
        {
            speed = 15;
        }
        if (Input.GetMouseButtonUp(0))
        {
            speed = 6;
        }
        if (count <= tempText.Length)
        {
            count += Time.deltaTime * speed;
            text.text = tempText.Substring(0,(int)count);
        }else if (count >= tempText.Length && !item.active)
        {
            Debug.Log("Item true 1 ");
            if (!flag)
            {
                item.SetActive(true);
                flag = true;
            }
        }
    }
    //public void End()
    //{
    //    count = tempText.Length;
    //}

}
